% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_quality_checks.R
\name{repeated_field_names}
\alias{repeated_field_names}
\title{Repeated field names}
\usage{
repeated_field_names(data_table)
}
\arguments{
\item{data_table}{table containing data from tidyxl::xlsx_cells output}
}
\value{
all records in `data_table` that correspond to repeated field names
}
\description{
Field names that appear multiple times in Excel source data sheets.
}
\seealso{
Other data_quality_issues: 
\code{\link{date_range_issues}()},
\code{\link{invalid_dates}()},
\code{\link{unclassified_field_names}()}
}
\concept{data_quality_issues}
