% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_quality_checks.R
\name{unclassified_field_names}
\alias{unclassified_field_names}
\title{Unclassified field names}
\usage{
unclassified_field_names(data_table, representative_test_categories)
}
\arguments{
\item{data_table}{table containing data from tidyxl::xlsx_cells output}

\item{representative_test_categories}{data frame containing the reference table}
}
\value{
all records in `data_table` that correspond to unclassified field names in tidyxl::xlsx_cells
format
}
\description{
Identify field names in Excel source data sheets that match regular expression patterns in
representative_categories.csv that have been labelled data quality issues. Additionally, find field
names that do not have a regular expression match, or have more that one match.
}
\seealso{
Other data_quality_issues: 
\code{\link{date_range_issues}()},
\code{\link{invalid_dates}()},
\code{\link{repeated_field_names}()}
}
\concept{data_quality_issues}
