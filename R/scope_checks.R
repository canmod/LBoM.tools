#' Assert Current Scope
#'
#' Scope of the data.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @param metadata result of \code{\link{get_tracking_metadata}}
#' @importFrom readr write_csv
#' @importFrom dplyr anti_join
#' @return filtered data list after entities that are out of scope have been removed. Data
#' is in tidyxl::xlsx_cells format where each record corresponds to one cell in source data.
#' @family scope
#' @export
assert_current_scope <- function(data_table, metadata){
  report_path = make_report_path(metadata)

  # check report_path directory exists, if not create it
  full_path <- file.path(".", report_path)

  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    print(paste0("Report folder path: ", full_path," created."))
  }

  # need to check valid input for data_table?
  
  inconsistent_sheets <- c('mort_uk__lon_1931-32_wk__rg',
                           'mort_uk__lon_1933-39_wk__rg',
                           'mort_uk__lon_1940-49_wk__rg',
                           'mort_uk__lon_1950_wk__rg'
  )
  
  data_table_sheets <- data_table %>% select(sheet) %>% unique() %>% pull()
  
  if (any(inconsistent_sheets %in% data_table_sheets)) {
    ## steps are 
    ## 1. filter data table for these recrods and duplicate, duplicate  c(year, month day records)
    affected_sheets <- (data_table
                        %>% filter(sheet %in% inconsistent_sheets)
                        )
    
    ## get column numbers of fields with these field names
    date_column_numbers <- (affected_sheets
                            %>% filter(character %in% c("year","month","day"))
                            %>% select(sheet,col)
                            %>% unique()
                            )
    
    ## max column numbers by sheet
    max_column_numbers <- (affected_sheets
                           %>% group_by(sheet)
                           %>% mutate(max_col=max(col))
                           %>% select(sheet, max_col)
                           %>% unique()
    )
    
    ## do I need to fix column numbers here, probably like below
    ## want 3 new column numbers
    filtered_field_names <- (affected_sheets
                             %>% right_join(date_column_numbers)
                            %>% filter(character %in% c("year","month","day"))
                            %>% uncount(2) ## does this duplicate records
                            %>% arrange(sheet,col)
                            ## rename these character fields to have suffixes
                            %>% mutate(suffix=if_else(row_number()%%2==0,".from",".to"))
                            %>% mutate(character=paste0(character,suffix))
                            %>% select(-suffix)
                            ## for odd rows add 3 to column number
                            ## %>% mutate(col=if_else(row_number()%%2==1,col+3,col))
                            )
    
    ## 2. duplicate numeric recrods and adjust column names and row numbers
    filtered_numeric_dates <- (affected_sheets
                               %>% right_join(date_column_numbers)
                               %>% filter( !(character %in% c("year","month","day")) & data_type=="numeric")
                               %>% uncount(2) ## does this duplicate records
                               %>% arrange(sheet,col,row)
                               %>% mutate(row=if_else(row_number()%%2==1,row+1,row))
                               ## remove extra records created above (last row of each sheet)
                               %>% group_by(sheet) 
                               %>% filter(row!=max(row)) 
                               %>% ungroup()
                               ## add first date (period_start_date) for each sheet
                               #%>% group_by(sheet)
                               #%>% mutate()
    )
    
    
    ## add first dates
    first_dates <- (filtered_numeric_dates
                    %>% group_by(sheet)
                    %>% filter(row==min(row))
                    %>% ungroup()
                    %>% arrange(sheet, col)
                    %>% mutate(time_info=if_else(row_number()%%3==1,'y',if_else(row_number()%%3==2,'m','d'))) 
                    %>% pivot_wider(names_from=time_info, values_from=numeric)
                    %>% group_by(sheet)
                    %>% mutate(y=year(convert_to_date(max(y,na.rm=TRUE),max(m,na.rm=TRUE),max(d,na.rm=TRUE),"end")-7),
                               m=month(convert_to_date(max(y,na.rm=TRUE),max(m,na.rm=TRUE),max(d,na.rm=TRUE),"end")-7),
                               d=day(convert_to_date(max(y,na.rm=TRUE),max(m,na.rm=TRUE),max(d,na.rm=TRUE),"end")-7),
                               numeric=if_else(row_number()%%3==1,y,if_else(row_number()%%3==2,m,d)))
                    %>% select(c(-y,-m,-d))
                    %>% ungroup()

    )
    
    filtered_numeric_dates <- rbind(filtered_numeric_dates,first_dates)
    
    ## update column numbers for new data
    updated_data <- (rbind(filtered_field_names,filtered_numeric_dates)
                 %>% left_join(max_column_numbers)
                 %>% arrange(sheet, row, col)
                 %>% mutate(col=if_else(row_number()%%6==2, max_col+1,
                                        if_else(row_number()%%6==4,max_col+2,
                                                if_else(row_number()%%6==0,max_col+3,col))),
                            address=if_else(row_number()%%6==2, 'ZZ1',
                                            if_else(row_number()%%6==4,'ZZ2',
                                                    if_else(row_number()%%6==0,'ZZ3',address))))
                 %>% select(-max_col)

                 
                 
                 
    )
    

    
    ## possible issue, addresses are not accurate
    ## does it matter about some duplicate things (is address field every used?)
    
    ## put all data together
    data_table<- (data_table
                  %>% anti_join(date_column_numbers)
                  %>% union(updated_data)
    )

    

  }

  # list of out of scope issues (not extensive)
  out_of_scope_list <- list(
    exclude_fields = exclude_fields(data_table),
    ## temporarily remove this (believe only mortality data is effected by this function)
    # inconsistent_column_names = inconsistent_column_names(data_table),
    empty_rows = empty_rows(data_table),
    empty_cols = empty_cols(data_table),
    missing_fields = missing_fields(data_table),
    header_rows = header_rows(data_table),
    question_marks = question_marks(data_table)
  )

  # all out of scope data
  out_of_scope <- bind_rows(out_of_scope_list)

  # save out of scope data to report_path location
  write_csv(out_of_scope, file = file.path(full_path, "out-of-scope.csv"))

  # in scope data
  in_scope <- anti_join(data_table, out_of_scope)

  return(in_scope)
}


# -------------------------------------------------------------
# Scope Functions
# -------------------------------------------------------------

#' Empty rows
#'
#' Identify entire rows in Excel sheets that contain no numeric or text data. These rows are outside the
#' boundaries of the data table contained in the sheet but they may contain Excel formatting that causes
#' the data reading functionality to perceive these rows as containing data.
#'
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @return all records in `data_table` that correspond to entire Excel sheet rows with no data
#' @family scope
#'
#' @export
empty_rows <- function(data_table){

  # get distinct rows per file and sheet that contain no data
  get_rows <- (data_table
               %>% select(file, sheet, row, is_blank)
               %>% group_by(file, sheet, row)
               %>% mutate(scope_reason=n_distinct(is_blank))
               %>% filter(is_blank==TRUE & scope_reason==1)
               %>% distinct()
               %>% mutate(scope_reason = "row contains no data")
               %>% select(-is_blank)
  )

  # all records that correspond to empty rows in get_rows
  get_records <- (data_table
                  %>% left_join(get_rows, by=c("file", "sheet","row"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}

#' Empty cols
#'
#' Identify entire columns in Excel sheets that contain no numeric or text data. These columns
#' are outside the boundaries of the data table contained in the sheet but they may contain Excel
#' formatting that causes the data reading functionality to perceive these columns as containing data.
#'
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to entire Excel sheet columns with no data
#' @family scope
#' @export
empty_cols <- function(data_table){

  # get distinct columns per file and sheet that contain no data
  get_cols <- (data_table
               %>% select(file, sheet, col, is_blank)
               %>% group_by(file, sheet, col)
               %>% mutate(scope_reason=n_distinct(is_blank))
               %>% filter(is_blank==TRUE & scope_reason==1)
               %>% distinct()
               %>% mutate(scope_reason = "column contains no data")
               %>% select(-is_blank)
  )

  # all records that correspond to empty columns in get_cols
  get_records <- (data_table
                  %>% left_join(get_cols, by=c("file", "sheet","col"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}

#' Missing fields
#'
#' Identify columns in Excel sheets that contain numeric data with no associated field name. Given
#' there is no field name the tidy data splitting functionality incorrectly assigns this data.
#'
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to entire Excel sheet columns with no field name
#' @family scope
#' @export
missing_fields <- function(data_table){

  # get distinct columns per file and sheet that contain no field name
  get_cols <- (data_table
               %>% select(file,sheet,col,data_type)
               %>% group_by(file,sheet,col,data_type)
               %>% summarize(count = n())
               %>% ungroup()
               %>% pivot_wider(names_from=data_type,values_from=count)
               %>% filter(is.na(character) & !is.na(numeric))
               %>% mutate(scope_reason = "missing field name")
               %>% select(file,sheet,col,scope_reason)
  )

  # all records that correspond to empty columns in get_cols
  get_records <- (data_table
                  %>% left_join(get_cols, by=c("file", "sheet","col"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}

#' Header rows
#'
#' Identify entire rows in Excel sheets that contain header comments in at least one
#' of the cells. These rows appear at the top of the Excel sheets and are assumed to
#' start with a hash tag character.
#'
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to header rows
#' @family scope
#' @export
header_rows <- function(data_table){

  # get rows containing header comments
  get_rows <- (data_table
               %>% filter(startsWith(character,"#"))
               %>% mutate(scope_reason = "row contains header comment")
               %>% select(file,sheet,row,scope_reason)
  )

  # all records that correspond to header rows in get_rows
  get_records <- (data_table
                  %>% left_join(get_rows, by=c("file", "sheet","row"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}

#' Exclude fields
#'
#' Temporarily exclude some fields that require additional work/thought on how
#' they should be incorporated in the tidy data set.
#' These include:
#' - parish total fields - need to determine how to relate parish groupings with specific parishes
#' - relative fields - fields that contain relative counts
#' - possibly need to remove fields with age data with no range (ends in a number)
#'
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to excluded fields
#' @family scope
#' @export
exclude_fields <- function(data_table){

  # get fields to exclude
  get_cols <- (data_table

               # TODO: important difference (buried vs christined)
               # possibly fixed -- naively used alternation group buried|christined
               # %>% filter(grepl("^(buried|christined)\\.in\\.([a-z]+\\.)*[0-9]+\\.([a-z]+\\.)*[a-z]+$|^increases([a-z]*\\.)*[a-z]+$|^decreases([a-z]*\\.)*[a-z]+$|^plague\\.in\\.([a-z]+\\.)*[0-9]+\\.([a-z]+\\.)*[a-z]+$",character,perl=TRUE))
               %>% filter(grepl("^(christined)\\.in\\.([a-z]+\\.)*[0-9]+\\.([a-z]+\\.)*[a-z]+$|^increases([a-z]*\\.)*[a-z]+$|^decreases([a-z]*\\.)*[a-z]+$",character,perl=TRUE))
               %>% mutate(scope_reason = "temporarily exclude field until method to incorporate in tidy table is known")
               %>% select(file,sheet,col,scope_reason)
  )

  # all records that correspond to get_cols
  get_records <- (data_table
                  %>% left_join(get_cols, by=c("file", "sheet","col"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}

#' Question marks
#'
#' Identify cells that contain question marks. It is not clear what type of missing data
#' is being represented by a question mark.
#'
#'
#' @param data_table table containing mortality data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to cells with question marks
#' @family scope
#' @export
question_marks <- function(data_table){

  # get cells containing question marks
  get_cells <- (data_table
               %>% filter(grepl("^([0-9])*\\?",character,perl=TRUE))
               %>% mutate(scope_reason = "question mark in cells")
               %>% select(file,sheet,row,col,scope_reason)
  )

  # all records that correspond to cells containing question marks
  get_records <- (data_table
                  %>% left_join(get_cells, by=c("file", "sheet","row","col"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}

#' Inconsistent time column names
#'
#' Identify sheets that contain time data formatted differently than all other sheets.
#' (One Excel file contains weekly time data with no ".from" or ".to" suffix for "year",
#' "month", and "day" columns.)
#'
#'
#' @param data_table table containing mortality data from tidyxl::xlsx_cells output
#' @return all records in `data_table` that correspond to inconsistent time column
#' names
#' @family scope
#' @export
inconsistent_column_names <- function(data_table){

  # get sheets that contain inconsistent time column names (all data in these sheets)
  get_sheets <- (data_table
                # using ^month$ as an identifier, could also use ^day$
                %>% filter(grepl("^month$",character,perl=TRUE))
                %>% mutate(scope_reason = "inconsistent time column names in this sheet")
                %>% select(file,sheet,scope_reason)
  )

  # all records that correspond to get_sheets
  get_records <- (data_table
                  %>% left_join(get_sheets, by=c("file", "sheet"), keep=FALSE)
                  %>% filter(!is.na(scope_reason))
  )
}
