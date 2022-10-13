#' Data quality filter
#'
#' Create data quality issue report from input table with the option to filter out data
#' quality issues.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @param metadata result of \code{\link{get_tracking_metadata}}
#' @param filter_data boolean flag to filter out data quality issues
#' @importFrom dplyr bind_rows
#' @importFrom stringr str_replace_all
#' @return Returns input `data_table` when `filter_data=FALSE`. If there are any
#' repeated field names in Excel sheets these field names will be appended with "_(cell address)"
#' to ensure field names are unique within Excel sheets. If `filter_data=TRUE` function will
#' return `data_table` after data quality issue records have been removed when .
#'
#' @export
filter_out_data_quality <- function(
    data_table,
    metadata,
    filter_data = TRUE,
    append_repeated_field_names = FALSE
  ){

  report_path = make_report_path(metadata)
  reference_table_list = getOption("reference_table_list")

  # defaults to TRUE
  # expecting choices to be a character vector, how to deal with boolean values?
  #filter_data=match.arg(filter_data)

  # check report_path directory exists, if not then create it
  full_path <- file.path(".", report_path)

  if (!dir.exists(full_path)) {
    dir.create(full_path)
    print(paste0("Report folder path: ", full_path," created."))
  }

 # check reference_path directory and files exists
  # representative_path <-file.path(".",reference_path,"representative-categories.csv")
  # if (!file.exists(representative_path)) stop(paste0("Reference table: ", representative_path, " not found."))
  #
  # description_path <-file.path(".",reference_path,"data-quality-description.csv")
  # if (!file.exists(description_path)) stop(paste0("Reference table: ", description_path, " not found."))

  # file contains descriptions for all data quality issues
  #data_quality_description <- read_csv(description_path)
  data_quality_description =
    LBoM.tools:::global_reference_tables$`data-quality-description`

  # list of data quality issues (not extensive)
  data_issues <- list(
    # no repeated field names were found
    # TODO: this line is commented out in some data types -- why?
    repeated_field_names = repeated_field_names(data_table),
    unclassified_field_names = unclassified_field_names(
      data_table,
      reference_table_list$`representative-categories`
    ),

    # these two are not necessary for some types (is it ok if they are present?)
    invalid_dates = invalid_dates(data_table), ## slow!
    date_range_issues = date_range_issues(data_table) ## slow!

  )

  # create data quality report
  data_quality_report <- (data_issues
    %>% bind_rows()
    %>% mutate(temp_data_quality = str_replace_all(data_quality, "[0-9]+", "#"))
    # add data quality descriptions
    %>% left_join(data_quality_description, by=c("temp_data_quality"="data_quality"))
    %>% select(-temp_data_quality)
  )

  # save data quality issues to report_path location
  write_csv(data_quality_report, file=file.path(".",report_path,"data-quality.csv"))

  if (filter_data) {
    output_data_table = anti_join(data_table, data_quality_report)
  } else if (append_repeated_field_names) {  # TODO: what is going on here?
    output_data_table = (data_table
    # TODO: not all data types have this join-mutate-select pipeline

    # no repeated field names were found
    %>% left_join(data_issues$repeated_field_names)
    # append cell address to repeated field names to allow for data processing
    %>% mutate(character=if_else(!is.na(data_quality) & data_type=="character",
                                 paste0(character,"_",address),
                                 character))
    %>% select(-field_name, -data_quality)
    )
  } else {
    output_data_table = data_table
  }
  return(output_data_table)
}



# -------------------------------------------------------------
# Data Quality Functions
# -------------------------------------------------------------

#' Convert to date
#'
#' Convert date string to date data type.
#'
#' @param y string containing 4-digit year
#' @param m string containing month numbers (1-12) or `NA`.
#' @param d string containing day numbers (1-31) or `NA`.
#' @param period a character string indicating whether the years are the
#' start or end of a period. This argument is only available when `m` and `d`
#' are `NA`. Valid inputs are "start"(January 1) or "end"(December 31),
#' default is NULL.
#' @importFrom lubridate ymd
#' @return Date object either a valid date or NA_Date_
#'
#' @export
convert_to_date <- function(y,m,d,period) {

  # convert date fields to a date string
  date_string <- make_date_string_vec(y, m, d, period)
  #date_string <- make_date_string(y, m, d, period)

  # convert date string to date, if warnings created from ymd() print warning message
  #date_type <- tryCatch(ymd(date_string),
  #                      warning = function(w) print(paste0("Not a valid date: ",date_string, " Converted to NA."))
  #)
  date_type = ymd(date_string)

  # if conversion fails set record to NA_Date_ to ensure field contains all the same data type
  if (is.character(date_type)) {
    date_type <- NA_Date_
  }

  return(date_type)
}



#' Invalid dates
#'
#' Identifies data records that correspond to invalid dates in Excel source data sheets. Invalid dates
#' refer to all dates that are `NA_Date_` after `create_date_fields()` was executed. For example,
#' non-existent dates (i.e.`2000-02-30`) are invalid dates.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @importFrom dplyr case_when
#' @return all records that correspond to invalid dates in `data_table`.
#' @family data_quality_issues
#' @export
invalid_dates <- function(data_table){

  # create fields of type Date in data_table
  dates <- create_date_fields(data_table)

  # get rows that correspond to invalid dates
  get_rows <- (dates
               %>% mutate(data_quality = case_when(is.na(period_start_date) ~ "invalid_period_start_date",
                                                   is.na(period_end_date) ~ "invalid_period_end_date",
                                                   TRUE ~ NA_character_))
               %>% filter(!is.na(data_quality))
               %>% select(file,sheet,row,data_quality)
  )

  # get all records that correspond to invalid dates
  get_records<- (data_table
                 %>% left_join(get_rows, by=c("file", "sheet","row"), keep=FALSE)
                 %>% filter(!is.na(data_quality))
  )
}




#' Date range issues
#'
#' Identify gaps and overlaps in weekly date ranges in Excel source data sheets.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr lead
#' @importFrom dplyr if_else
#' @return all records in `data_table` that correspond to gaps and overlaps in weekly date range data
#' @family data_quality_issues
#' @export
date_range_issues <- function(data_table){

  # create date fields
  dates <- create_date_fields(data_table)

  # get all rows that correspond to date range issues
  get_rows <- (dates
     # remove invalid dates
     %>% filter(!is.na(period_start_date))
     %>% filter(!is.na(period_end_date))
     %>% arrange(file,sheet,period_start_date)
     %>% group_by(file,sheet)
     # compute date difference from end of current week to start of previous week
     %>% mutate(date_diff = period_end_date-lead(period_start_date))
     %>% filter(date_diff!=0)
     %>% mutate(data_quality = if_else(date_diff > 0, paste0("weekly_date_range_overlap_(n=",abs(date_diff),")"),
                                       paste0("weekly_date_range_gap_(n=",abs(date_diff),")")))
     %>% select(file,sheet,row,data_quality)
  )

  # get all records that correspond to date range issues
  get_records<- (data_table
   %>% left_join(get_rows, by=c("file", "sheet","row"), keep=FALSE)
   %>% filter(!is.na(data_quality))
  )

}


#' Repeated field names
#'
#' Field names that appear multiple times in Excel source data sheets.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to repeated field names
#' @family data_quality_issues
#' @export
repeated_field_names <- function(data_table){

  # get field names that appear multiple times in the same excel sheet
  get_field_names <- (data_table
                      %>% select(character, row, col, sheet, file)
                      # what about time metadata? should this be excluded from character
                      # search
                      %>% filter(!is.na(character))
                      %>% group_by(sheet,character)
                      %>% mutate(data_quality=n())
                      %>% filter(data_quality>1)
                      %>% mutate(data_quality=paste0("repeated_field_name_(n=",as.character(data_quality),")"))
                      %>% ungroup()
                      %>% rename(field_name=character)
                      %>% select(-row)
  )

  # all records that correspond to repeated field names in get_field_names
  get_records<- (data_table
                 %>% left_join(get_field_names, by=c("file", "sheet","col"), keep=FALSE)
                 %>% filter(!is.na(data_quality))
  )
}

#' Unclassified field names
#'
#' Identify field names in Excel source data sheets that match regular expression patterns in
#' representative_categories.csv that have been labelled data quality issues. Additionally, find field
#' names that do not have a regular expression match, or have more that one match.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @param representative_test_categories data frame containing the reference table
#' @importFrom readr read_csv
#' @importFrom tidyr expand
#' @importFrom dplyr union
#' @return all records in `data_table` that correspond to unclassified field names in tidyxl::xlsx_cells
#' format
#' @family data_quality_issues
#' @export
unclassified_field_names <- function(data_table, representative_test_categories){


  # file contains all regex patterns for data
  # this list is meant to be exhaustive so all categories match only
  # one regex pattern in this file
  #representative_test_categories <- read_csv(reference_table)

  # match field names with regex patterns in representative_test_categories
  pattern_matches <- (data_table
    %>% select(character)
    # what about time metadata ?
    %>% filter(!is.na(character))
    %>% expand(character, pattern = representative_test_categories$validation_pattern)
    %>% rowwise()
    %>% mutate(match_pattern = grepl(pattern, character, perl=TRUE))
    %>% ungroup()
  )

  # check for no regex matches and multiple regex matches in field names
  miss_matches <- (pattern_matches
   %>% group_by(character)
   %>% mutate(num_matches = sum(match_pattern))
   %>% mutate(data_quality = case_when(
         num_matches < 1 ~ "inconclusive_data_quality_test", # no match
         num_matches > 1 ~ "multiple_known_data_quality_issues", # multiple matches
         TRUE~ NA_character_))
   %>% ungroup()
   %>% filter(!is.na(data_quality))
   %>% select(character, data_quality)
   %>% distinct()
  )

  # for remaining field names with exactly one regex match, get those field
  # names with data quality issues
  one_match <- (pattern_matches
    %>% anti_join(miss_matches)
    %>% filter(match_pattern==TRUE)
    %>% left_join(representative_test_categories, by=c("pattern"="validation_pattern"))
    %>% select(character, data_quality)
    %>% filter(!is.na(data_quality))
    %>% distinct()
  )

  # TODO: this condition is not checked for all data types -- why?
  if (nrow(one_match) > 0 | nrow(miss_matches) > 0){
    # all field names with regex data quality issues
    # (i.e. including miss_matches and one_match)
    get_field_names <- (data_table
      %>% select(character, row, col, sheet, file)
      %>% left_join(union(miss_matches, one_match),by="character")
      %>% filter(!is.na(data_quality))
      %>% select(-row)
      %>% rename(field_name=character)
    )

    # all records that correspond to field name data quality issues in get_field_names
    get_records <-(data_table
     %>% left_join(get_field_names, by=c("file", "sheet","col"), keep=FALSE)
     %>% filter(!is.na(data_quality))
    )
  }
}
