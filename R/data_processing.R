#' Process Excel Sheet
#'
#' Process excel sheet to convert from tidyxl::xlsx_cells format to tidy long format
#'
#' @param sheet table containing one Excel sheet in tidyxl::xlsx_cells format
#' @param col_time_metadata time metadata fields with "col_" prefix
#' @param address_time_metadata time metadata fields with "address_" prefix
#' @param numeric_time_metadata time metadata fields with "numeric_" prefix
#' @param data_type TODO: describe data type
#' @importFrom unpivotr behead
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr summarize
#' @importFrom dplyr rename_all
#' @importFrom stringr str_replace regex
#'
#' @return processed sheet as a tibble
#'
#' @export
process_sheet <- function(
    sheet,
    col_time_metadata,
    address_time_metadata,
    numeric_time_metadata,
    data_type = c("mortality", "all-cause-mortality", "births", "plague", "population")) {

  data_type = match.arg(data_type)

  numeric_field_name = switch(
    as.character(data_type %in% c("mortality", "all-cause-mortality")),
    `TRUE` = "cause",
    `FALSE` = "total"
  )

  # get additional fields to be joined later
  additional_fields <- (sheet
                      %>% select(address, comment, sheet, file, data_category)
  )

  sheet = try(sheet
      %>% behead("N",col_names)
      # assumes data is numeric, what if there is logical, text or date data types
      # in data cells?
      %>% select (row, col, address, character, numeric, col_names)
      %>% pivot_wider(id_cols = c(row),
                      names_from = c(col_names),
                      values_from = c(numeric,col,address))
      %>% select(-any_of(col_time_metadata),
                 -any_of(address_time_metadata))
      %>% pivot_longer(cols = c(-any_of(numeric_time_metadata),-row),
                       values_transform = list(value = as.character))

      # TODO: important difference between data types
      # possibly fixed with numeric_field_name
      %>% extract(name,into=c(NA,numeric_field_name),
                  regex = "^(col|numeric|address)_(.*)",remove=FALSE)
      %>% group_by(across(c(-name, -value)))
      %>% summarize(col = value[startsWith(name, "col_")],
                    address = value[startsWith(name, "address_")],
                    # TODO: count here is deaths for other data types
                    count = value[startsWith(name, "numeric_")])
      %>% rename_all(~str_replace(.,regex("^numeric_", ignore_case = TRUE), ""))
      # using inner join, for cells that have been identified as out of scope
      # the address will be NA
      %>% inner_join(additional_fields, by="address")
      %>% ungroup()
  )
  # if ((data_type == "mortality") & !inherits(sheet, "try-error")) {
  #   sheet = rename(sheet, deaths = count)
  # } else {
  # }
  missing_columns = setdiff(sub("^numeric_", "", numeric_time_metadata), names(sheet))
  for (col in missing_columns) {
    sheet[[col]] = NA_real_
  }
  sheet
}

#' Process Data
#'
#' Process all data to convert from tidyxl::xlsx_cells format to long format.
#' The output of this function contains data provenance information that
#' allow one to trace each record back to a particular cell in the
#' digitized Excel data.
#'
#' @param data_table table containing data in tidyxl::xlsx_cells format
#' @param metadata result of \code{\link{get_tracking_metadata}}
#'
#' @importFrom glue glue
#' @importFrom dplyr group_split
#' @importFrom dplyr relocate
#'
#' @return table containing counts in long format (by sex if applicable) and
#' compact time metadata format, age data extracted from cause field names
#' (if applicable)
#' @name process
NULL

assert_report_path = function(file_path) {
  # check report_path directory exists
  full_path <-file.path(".",file_path)
  if (!dir.exists(full_path)) stop(paste0("File path: ", full_path, " not found."))
  return(full_path)
}

#' @describeIn process Process population data
#' @export
process_population <- function(data_table, metadata) {
  file_path = make_report_path(metadata)
  full_path = assert_report_path(file_path)
  file_name = metadata$TidyDataset$tidy_dataset
  reference_table_list = getOption("reference_table_list")

  # get all fields in data_table that pertain to time metadata
  time_metadata <- (data_table
    %>% select(character)
    %>% filter(!is.na(character))
    %>% rowwise()
    %>% filter(grepl("^(?=year|month|day|week|Week|numdate).*", character, perl = TRUE))
    %>% ungroup()
    %>% unlist()
    %>% unique()
  )
  # add "col_", "address_", and "numeric_" prefix to time metadata
  col_time_metadata <- as.character(glue("col_{time_metadata}"))
  address_time_metadata <- as.character(glue("address_{time_metadata}"))
  numeric_time_metadata <- as.character(glue("numeric_{time_metadata}"))

  # split data_table into list of Excel sheets
  split_data <- (data_table
                 %>% group_split(file,sheet)
  )

  # apply process_sheet for each Excel sheet
  processed_data <-lapply(split_data, function(x){
    process_sheet(
      x,
      col_time_metadata,
      address_time_metadata,
      numeric_time_metadata,
      "population"
    )
  })

  # final processed data in long format
  final_data <- (processed_data
    %>% bind_rows()
    #%>% rowwise()
    %>% mutate(period_start_date = make_date_string_vec_year(year, NA, NA, "start"))
    %>% mutate(period_end_date = make_date_string_vec_year(year,NA, NA, "end"))
    #%>% ungroup()
    %>% select(-any_of(time_metadata))
    %>% mutate(sex = if_else(grepl("^.*(?=(fe)?male).*$",total,perl=TRUE),
                             total,
                             NA_character_))
    %>% rename(population=count)
    %>% select(-total)
    %>% relocate(
      data_category, file, sheet, row, col, address, comment,
      period_start_date, period_end_date, sex, population
    )
  )

  saveRDS(final_data, file = paste0(file.path(full_path, file_name), ".rds"))
  final_data
}

#' @describeIn process Process all-cause mortality data
#' @export
process_all_cause <- function(data_table, metadata) {
  file_path = make_report_path(metadata)
  full_path = assert_report_path(file_path)
  file_name = metadata$TidyDataset$tidy_dataset
  reference_table_list = getOption("reference_table_list")

  # get all fields in data_table that pertain to time metadata
  time_metadata <- (data_table
                    %>% select(character)
                    %>% filter(!is.na(character))
                    %>% rowwise()
                    %>% filter(grepl("^(?=year|month|day|week|Week|numdate).*",character,perl=TRUE))
                    %>% ungroup()
                    %>% unlist()
                    %>% unique()
  )
  # add "col_", "address_", and "numeric_" prefix to time metadata
  col_time_metadata <- as.character(glue("col_{time_metadata}"))
  address_time_metadata <- as.character(glue("address_{time_metadata}"))
  numeric_time_metadata <- as.character(glue("numeric_{time_metadata}"))

  # split data_table into list of Excel sheets
  split_data <- (data_table
                 %>% group_split(file,sheet)
  )

  # apply process_sheet for each Excel sheet
  processed_data <- lapply(split_data, function(x){
    process_sheet(
      x,
      col_time_metadata,
      address_time_metadata,
      numeric_time_metadata,
      "all-cause-mortality"
    )
  })

  # read reference tables
  parish_data <- reference_table_list$parishes
  disease_data <- reference_table_list$`disease-family`

  # final processed data in long format
  final_data <- (processed_data
    %>% bind_rows()
    %>% mutate(year.from = if_else(is.na(year),year.from, year),
               year.to = if_else(is.na(year), year.to, year))
    #%>% rowwise()
    %>% mutate(period_start_date = make_date_string_vec(year.from, month.from, day.from,"start"))
    %>% mutate(period_end_date = make_date_string_vec(year.to, month.to, day.to,"end"))
    #%>% ungroup()
    %>% select(-any_of(time_metadata))
    %>% mutate(lower_age = get_lower_lookup("all_cause")(cause))
    %>% mutate(upper_age = get_upper_lookup("all_cause")(cause))
    %>% left_join(disease_data, by=c("cause"="category"))
    %>% mutate(cause = iidda::memoise_remove_age(cause, get_re_templates("all_cause"), prefix="\\.age\\."))
    %>% mutate(cause = if_else(!is.na(disease_family),NA_character_,cause))
    %>% left_join(parish_data, by=c("cause"="synonym"))
    %>% mutate(cause = if_else(!is.na(parish),NA_character_,cause))
    %>% mutate(sex = if_else(grepl("^.*(?=(fe)?male).*$",cause,perl=TRUE),
                             cause,
                             NA_character_))
    %>% mutate(cause = if_else(!is.na(sex),NA_character_,cause))
    %>% mutate(burials = if_else(grepl("buried",cause,perl=TRUE),
                                 count,
                                 NA_character_))
    %>% mutate(specified_cause_deaths = if_else(grepl("specified",cause,perl=TRUE),
                                 count,
                                 NA_character_))
    %>% mutate(all_cause_deaths = if_else(is.na(burials) & is.na(specified_cause_deaths),count,NA_character_))
    %>% select(-count,-cause)
    %>% relocate(data_category, file, sheet, row, col, address, comment, period_start_date,
                 period_end_date, lower_age, upper_age,
                 sex, parish, disease_family, all_cause_deaths, specified_cause_deaths, burials)

  )

  saveRDS(final_data, file=paste0(file.path(full_path,file_name),".rds"))
  final_data

}

#' @describeIn process Process birth data
#' @export
process_births <- function(data_table, metadata) {
  file_path = make_report_path(metadata)
  full_path = assert_report_path(file_path)
  file_name = metadata$TidyDataset$tidy_dataset
  reference_table_list = getOption("reference_table_list")

  # get all fields in data_table that pertain to time metadata
  time_metadata <- (data_table
                    %>% select(character)
                    %>% filter(!is.na(character))
                    %>% rowwise()
                    %>% filter(grepl("^(?=year|month|day|week|Week|numdate).*",character,perl=TRUE))
                    %>% ungroup()
                    %>% unlist()
                    %>% unique()
  )
  # add "col_", "address_", and "numeric_" prefix to time metadata
  col_time_metadata <- as.character(glue("col_{time_metadata}"))
  address_time_metadata <- as.character(glue("address_{time_metadata}"))
  numeric_time_metadata <- as.character(glue("numeric_{time_metadata}"))

  # split data_table into list of Excel sheets
  split_data <- (data_table
                 %>% group_split(file,sheet)
  )

  # apply process_sheet for each Excel sheet
  processed_data <-lapply(split_data, function(x){
    process_sheet(
      x,
      col_time_metadata,
      address_time_metadata,
      numeric_time_metadata,
      "births"
    )
  })
  # read reference tables
  parish_data <- reference_table_list$parishes

  # final processed data in long format
  final_data <- (processed_data
    %>% bind_rows()
    %>% mutate(year.from = if_else(is.na(year),year.from, year),
               year.to = if_else(is.na(year), year.to, year))
    #%>% rowwise()
    %>% mutate(period_start_date = make_date_string_vec(year.from, month.from, day.from,"start"))
    %>% mutate(period_end_date = make_date_string_vec(year.to, month.to, day.to,"end"))
    #%>% ungroup()
    %>% select(-any_of(time_metadata))
    %>% left_join(parish_data, by=c("total"="synonym"))
    %>% mutate(total = if_else(!is.na(parish),NA_character_,total))
    %>% mutate(sex = if_else(grepl("^.*(?=(fe)?male).*$",total,perl=TRUE),
                             total,
                             NA_character_))
    %>% mutate(total = if_else(!is.na(sex),NA_character_,total))
    %>% mutate(births = if_else(grepl("birth",total,perl=TRUE),
                                 count,
                                 NA_character_))
    %>% mutate(christened = if_else(is.na(births),
                                                count,
                                                NA_character_))
    %>% select(-count,-total)
    %>% relocate(data_category, file, sheet, row, col, address, comment, period_start_date,
                 period_end_date, sex, parish, births, christened)

  )

  saveRDS(final_data, file=paste0(file.path(full_path,file_name),".rds"))
  final_data
}

#' @describeIn process Process plague data
#' @export
process_plague <- function(data_table, metadata) {
  file_path = make_report_path(metadata)
  full_path = assert_report_path(file_path)
  file_name = metadata$TidyDataset$tidy_dataset
  reference_table_list = getOption("reference_table_list")

  # get all fields in data_table that pertain to time metadata
  time_metadata <- (data_table
                    %>% select(character)
                    %>% filter(!is.na(character))
                    %>% rowwise()
                    %>% filter(grepl("^(?=year|month|day|week|Week|numdate).*",character,perl=TRUE))
                    %>% ungroup()
                    %>% unlist()
                    %>% unique()
  )
  # add "col_", "address_", and "numeric_" prefix to time metadata
  col_time_metadata <- as.character(glue("col_{time_metadata}"))
  address_time_metadata <- as.character(glue("address_{time_metadata}"))
  numeric_time_metadata <- as.character(glue("numeric_{time_metadata}"))

  # split data_table into list of Excel sheets
  split_data <- (data_table
                 %>% group_split(file,sheet)
  )

  # apply process_sheet for each Excel sheet
  processed_data <-lapply(split_data, function(x){
    process_sheet(
      x,
      col_time_metadata,
      address_time_metadata,
      numeric_time_metadata,
      "plague"
    )
  })

  # read reference tables
  parish_data <- reference_table_list$parishes

  # final processed data in long format
  final_data <- (processed_data
    %>% bind_rows()
    #%>% rowwise()
    %>% mutate(period_start_date = make_date_string_vec(year.from, month.from, day.from,"start"))
    %>% mutate(period_end_date = make_date_string_vec(year.to, month.to, day.to,"end"))
    #%>% ungroup()
    %>% select(-any_of(time_metadata))
    %>% left_join(parish_data, by=c("total"="synonym"))
    %>% mutate(total = if_else(!is.na(parish),NA_character_,total))
    %>% mutate(sex = if_else(grepl("^.*(?=(fe)?male).*$",total,perl=TRUE),
                             total,
                             NA_character_))
    %>% mutate(total = if_else(!is.na(sex),NA_character_,total))
    %>% mutate(not_infected_parishes = if_else(grepl("clear",total,perl=TRUE),
                                           count,
                                           NA_character_))
    %>% mutate(infected_parishes = if_else(grepl("infected",total,perl=TRUE),
                                count,
                                NA_character_))
    %>% mutate(deaths = if_else(is.na(not_infected_parishes) & is.na(infected_parishes),
                                    count,
                                    NA_character_))
    %>% select(-count,-total)
    %>% relocate(data_category, file, sheet, row, col, address, comment, period_start_date,
                 period_end_date, sex, parish, not_infected_parishes, infected_parishes,deaths)

  )

  saveRDS(final_data, file=paste0(file.path(full_path,file_name),".rds"))
  final_data
}

#' @describeIn process Process mortality data
#' @export
process_mortality <- function(data_table, metadata) {
  file_path = make_report_path(metadata)
  full_path = assert_report_path(file_path)
  file_name = metadata$TidyDataset$tidy_dataset
  reference_table_list = getOption("reference_table_list")

  # get all fields in data_table that pertain to time metadata
  time_metadata <- (data_table
                    %>% select(character)
                    %>% filter(!is.na(character))
                    %>% rowwise()
                    %>% filter(grepl("^(?=year|month|day|week|Week|numdate).*",character,perl=TRUE))
                    %>% ungroup()
                    %>% unlist()
                    %>% unique()
                    )

  # add "col_", "address_", and "numeric_" prefix to time metadata
  col_time_metadata <- as.character(glue("col_{time_metadata}"))
  address_time_metadata <- as.character(glue("address_{time_metadata}"))
  numeric_time_metadata <- as.character(glue("numeric_{time_metadata}"))

  # split data_table into list of Excel sheets
  split_data <- (data_table
                 %>% group_split(file,sheet)
                 )

  # apply process_sheet for each Excel sheet
  processed_data <- lapply(split_data, function(x){
    process_sheet(
      x,
      col_time_metadata,
      address_time_metadata,
      numeric_time_metadata,
      "mortality"
    )
  })

  # final processed data in long format
  final_data <- (processed_data
                 %>% bind_rows()
                 %>% mutate(year.from = if_else(is.na(year),year.from, year),
                            year.to = if_else(is.na(year), year.to, year))
                 #%>% rowwise()
                 %>% mutate(period_start_date = make_date_string_vec(year.from, month.from, day.from,"start"))
                 %>% mutate(period_end_date = make_date_string_vec(year.to, month.to, day.to,"end"))
                 #%>% ungroup()
                 %>% select(-any_of(time_metadata))
                 # extract age information for category
                 %>% mutate(lower_age = get_lower_lookup("mortality")(cause))
                 %>% mutate(upper_age = get_upper_lookup("mortality")(cause))
                 # remove age information from category
                 %>% mutate(cause = iidda::memoise_remove_age(cause, get_re_templates("mortality"), prefix="\\.age\\."))
                 # order fields
                 %>% relocate(data_category, file, sheet, row, col, address, comment, period_start_date,
                              period_end_date, lower_age, upper_age,
                              cause, deaths)
                 )

  saveRDS(final_data, file=paste0(file.path(full_path,file_name),".rds"))
  final_data
}

#' Fix Tidy Data Columns
#'
#' @param data_set processed output from one of the
#' data processing functions (e.g. \code{\link{process_mortality}})
#' @importFrom purrr keep discard
#' @importFrom dplyr select_if
#' @export
fix_tidy_data_columns = function(data_set) {
  # split RDS file into mutually exclusive tables
  (data_set
     %>% mutate(time_scale = factor(if_else(grepl("^.*(?=wk)",sheet,perl=TRUE),"wk","an")))
     # keep time_scale in final output
     # creating two identical time variables so one can be dropped
     %>% mutate(time_period = time_scale)
     %>% mutate(age_class = if("lower_age" %in% colnames(.)) factor(if_else(is.na(lower_age) & is.na(upper_age),"-","age")) else "-")
     # parish groups have been deemed out of scope currently
     %>% mutate(parish_class = if("parish" %in% colnames(.)) factor(if_else(is.na(parish),"-","par")) else "-")
     %>% mutate(sex_class = if("sex" %in% colnames(.)) factor(if_else(is.na(sex),"-","sex")) else "-")
     %>% mutate(disease_class = if("disease_family" %in% colnames(.)) factor(if_else(is.na(disease_family),"-","df")) else "-")
     # %>% group_split.(time_period, age_class,
     #                  parish_class, sex_class, disease_class, .named=TRUE, .keep=FALSE)
     #
     %>% discard(~all(is.na(.)))
     # # removes empty tables
     # %>% discard(function(x) nrow(x) == 0)
     # # remove empty columns
     # #%>% compact()
     # #%>% discard(function(x) )
     #
     %>% keep(~!(all(is.na(.))))
     %>% select_if(~!(all(is.na(.))))
  )
}
