lbom_path_utility = function(
    source_data_folder,
    intermediate_data_folder,
    data_type = c("mortality", "all-cause-mortality", "births", "plague", "population")
  ) {

  data_type = match.arg(data_type)

  # path to Excel xlsx data files in working directory
  data_path <- file.path(".", source_data_folder)

  # check source_data_folder exists
  if (!dir.exists(data_path)) stop(paste0("Source data folder path: ", data_path, " not found."))

  # path for output file
  output_path <- file.path(".",intermediate_data_folder, data_type)

  # check output_path exists
  if (!dir.exists(output_path)) stop(paste0("Output data folder path: ", output_path, " not found."))

  # get xlsx file names in data_path
  file_names <- dir(data_path, pattern =".xlsx", full.names=TRUE)
  attr(file_names, "output_path") = output_path
  file_names
}

lbom_sheet_names = function(file_names) {
  # get sheet names in files
  (sapply(file_names,USE.NAMES = TRUE ,xlsx_sheet_names)
    %>% stack()
  )
}

#' @export
lbom_pre_processing = function(data, metadata) {
  data = (data
           # Data pre-processing
           # remove character_formatted (this field is a list containing Excel formatting
           # for character data types ex. bold text or coloured cells)
           # Excel fomatting may be captured in later iterations of tidy data
           %>% select(-character_formatted)
           # remove white space in character field
           %>% mutate(character = trimws(character))
           # convert empty strings to NA
           %>% mutate(character = if_else((character==""),NA_character_,character))
           # convert character field to lower case (this includes header comments)
           %>% mutate(character = tolower(character))
        )
  if (!is.null(metadata$lbom_info)) {
    data = (data
      %>% mutate(file = as.character(metadata$lbom_info$relative_path))
      %>% mutate(data_category = metadata$lbom_info$data_category)
    )
  }
  data
}

clean_and_combine_digitized_data = function(sheet_name_table) {
  # read in all data from cells
  all_data <- apply(sheet_name_table, 1, function(x){
    (xlsx_cells(path = x["ind"], sheets=x["values"])
      %>% lbom_pre_processing(list(relative_path = x["ind"]))
    )
  }
  )
  names(all_data) <- sheet_name_table$values
  all_data
}

#' Split Source Excel
#'
#' Read in source .xlsx data files and perform some data pre-processing.
#' Splits data into data categories and saves resulting table as RDS file.
#'
#' @param source_data_folder folder where source data is saved.
#' @param intermediate_data_folder top level folder for output, resulting RDS output file
#' will be saved in intermediate_data_folder > data_type location.
#' @param data_type data type to save as RDS file. Valid inputs are
#'  mortality,
#'  all-cause-mortality,
#'  births,
#'  plague,
#'  population.
#'  The default is \code{mortality}.
#'
#' @return the combined data frame that is saved in an RDS file
#' @importFrom tidyxl xlsx_cells xlsx_sheet_names
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate case_when bind_rows select if_else
#'
#' @export
#'
split_source_excel <- function(
    source_data_folder,
    intermediate_data_folder,
    data_type = c("mortality", "all-cause-mortality", "births", "plague", "population")
  ) {

  data_type = match.arg(data_type)

  file_names = lbom_path_utility(source_data_folder, intermediate_data_folder, data_type)

  sheet_names = lbom_sheet_names(file_names)

  all_data = clean_and_combine_digitized_data(sheet_names)

  # group sheets based on sheet name into data categories:
  #   mortality = mortality by disease
  #   all-cause-mortality = all-cause mortality
  #   births = births
  #   plague = plague deaths
  #   population
  # Regular expressions were modified from categorize_data() in
  # create_masterList.R which assumes source data excel sheets have
  # been named with consistent data identifier information.
  sheet_grouping <- case_when(
    grepl("^mort(?!.*1728\\-1829).*(?<!par_p)$",names(all_data), perl=TRUE)~"mortality",
    grepl("^acm.*$",names(all_data), perl=TRUE)~"all-cause-mortality",
    grepl("^(?=.*par_p)(?!.*plag|bth).*$",names(all_data), perl=TRUE)~"all-cause-mortality",
    grepl("^(?=.*1728\\-1829).*$",names(all_data), perl=TRUE)~"all-cause-mortality",
    grepl("^bth.*$",names(all_data), perl=TRUE)~"births",
    grepl("^plag.*$",names(all_data), perl=TRUE)~"plague",
    grepl("^pop.*$",names(all_data), perl=TRUE)~"population",
    TRUE~"unknown-data-category"
  )

  # split all data based on data categories
  sheet_split <- split(all_data, as.factor(sheet_grouping))

  # bind data within data categories
  bind_sheets <- lapply(seq_along(sheet_split),function(i){
    (sheet_split[[i]]
     %>% bind_rows
     %>% mutate(data_category=names(sheet_split[i]))
     )
    }
    ) %>% setNames(names(sheet_split))

  combined_data = bind_sheets[[data_type]]
  # save RDS file for input data_type
  saveRDS(
    combined_data,
    file = (file.path(".", attr(file_names, "output_path"), paste0(data_type,".rds")))
  )

  return(combined_data)
}



#' Read Source RDS
#'
#' Read in source RDS data
#'
#' @param source_data_folder top level folder where source RDS files are saved
#' @param data_category data category
#'
#' @return tibble containing all data in specified data category
#'
#' @export
#'
read_source_RDS <- function(source_data_folder,
                            data_category=list(
                              "mortality",
                              "acm",
                              "birth",
                              "plague",
                              "population",
                              "unknown_data_category")) {
  # path to RDS source data
  data_path <-file.path(".",source_data_folder,paste0(data_category,".rds"))

  # check file in source_data_folder exists
  if (!file.exists(data_path)) stop(paste0("Source data:", data_path, " not found."))

  # read in file
  readRDS(data_path)

}

#' Make Time Metadata
#'
#' @param data_table Data frame returned by `read_digitized_data`.
#'
#' @export
make_time_metadata = function(data_table) {
  time_metadata <- (data_table
    %>% select(character)
    %>% distinct()
    %>% filter(!is.na(character))
    %>% filter(grepl(
      "^(?=year|month|day|week|Week|numdate).*",
      character,
      perl = TRUE
    ))
    %>% unlist()
  )
  list(
    time_metadata = time_metadata,
    col_time_metadata = as.character(glue("col_{time_metadata}")),
    address_time_metadata = as.character(glue("address_{time_metadata}")),
    numeric_time_metadata = as.character(glue("numeric_{time_metadata}"))
  )
}

#' Combine Sheets
#'
#' @param data_table Data frame returned by `read_digitized_data`.
#' @param time_metadata List returned by \code{\link{make_time_metadata}}.
#'
#' @export
combine_sheets = function(data_table, time_metadata, dataset_type) {
  split_data <- (data_table %>% group_split(file, sheet))
  processed_data <- lapply(split_data, function(x) {
    process_sheet(x
      , time_metadata$col_time_metadata
      , time_metadata$address_time_metadata
      , time_metadata$numeric_time_metadata
      , dataset_type
    )
  })
  ## since there are no reshape ops in the `final_data` pipeline below,
  ## and given that i think that we do not want rows with missing counts,
  ## i'm going to remove all missing entries in `count` after binding
  ## the sheets together
  (processed_data
    %>% bind_rows
    %>% filter(!is.na(count))
  )
}
