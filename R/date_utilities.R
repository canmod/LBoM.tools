#' Create date fields
#'
#' Creates two new fields of class `Date` named `period_start_date` and `period_end_date` in
#' input table using existing fields containing date range information.
#'
#' @param data_table table containing data from tidyxl::xlsx_cells output
#' @importFrom dplyr right_join
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr rowwise
#' @importFrom dplyr ungroup
#' @importFrom dplyr rename
#' @return `data_table` containing additional fields named `period_start_date` and `period_end_date`
#'
#' @export
create_date_fields <- function(data_table){

  dates <- (data_table
      # get cols that contain time data
      %>% select(character, col, sheet, file)
      %>% filter(grepl("^.*(?=wk)",sheet,perl=TRUE))
      # time columns
      %>% filter(grepl("^(?=year|day|month)",character,perl=TRUE))
      %>% rename(time_col=character)
      %>% right_join(in_scope_data, by=c("file","sheet","col"))
      %>% filter(!is.na(time_col))
      %>% select(file, sheet, row, col, time_col, numeric)
      %>% filter(!is.na(numeric))
      %>% pivot_wider(id_cols=c(file,sheet,row),names_from=time_col, values_from=numeric)
      # create date fields
      # %>% rowwise()
      %>% {if("year.from" %in% names(.)) mutate(.,period_start_date = convert_to_date(year.from, month.from, day.from,"start"))
        else if("year" %in% names(.)) mutate(.,period_start_date = convert_to_date(year, NA, NA,"start"))
        else .}
      %>% {if("year.to" %in% names(.)) mutate(.,period_end_date = convert_to_date(year.to, month.to, day.to,"end"))
        else if("year" %in% names(.)) mutate(.,period_end_date = convert_to_date(year, NA, NA,"end"))
        else .}
  )
}



#' Concatenate Year, Month, and Day Fields into a yyyy-mm-dd String.
#'
#' @param y vector containing years
#' @param m vector containing month numbers (1-12) or `NA`.
#' @param d vector containing day-of-month numbers (1-31) or `NA`.
#' @param period a character string indicating whether the years are the
#' start or end of a period. This argument is only available when `m` and `d`
#' are `NA`. Valid inputs are "start"(January 1) or "end"(December 31),
#' default is NULL.
#'
#'
#'
make_date_string = function(y,m,d,period=NULL) {
  y = as.integer(y)
  m = as.integer(m)
  d = as.integer(d)

  # FIXME: this check should be smarter (e.g. feb 30 should not be allowed)
  # should lubridate be used here?
  stopifnot((between(m, 1, 12) & (between(d, 1, 31))) | (is.na(m) & is.na(d)))

  stopifnot(is.null(period) | (period %in% c("start","end")))

  if (is.na(m)) {
    if (is.null(period)) {
      stop("Argument `period` not provided.")
    } else if (period=="end") {
      paste(
        sprintf('%04d', y),
        "12",
        "31", sep = '-'
      )
    } else if (period=="start") {
      paste(
        sprintf('%04d', y),
        "01",
        "01", sep = '-'
      )
    }
  }
  else {
    paste(
      sprintf('%04d', y),
      sprintf('%02d', m),
      sprintf('%02d', d),
      sep = '-'
    )
  }
}

#' @describeIn convert_to_date Vectorized version of date conversion
#' @export
convert_vec_to_date = Vectorize(
  convert_to_date,
  vectorize.args = c("y", "m", "d")
)

# make_date_string_vec = Vectorize(
#   make_date_string,
#   vectorize.args = c("y", "m", "d")
# )

#' @export
make_date_string_vec = function(y,m,d,period=NULL) {
  y = as.integer(y)
  m = as.integer(m)
  d = as.integer(d)
  is_na_m = is.na(m)
  if (period == "start") {
    m[is_na_m] = d[is_na_m] = 1
  } else if (period == "end") {
    m[is_na_m] = 12
    d[is_na_m] = 31
  }
  as.character(lubridate::ymd(paste(y, m, d, sep = "-")))
}

#' @export
make_date_string_vec_year = Vectorize(
  make_date_string,
  vectorize.args = "y"
)
