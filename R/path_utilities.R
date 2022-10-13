#' Make Report Path
#'
#' Make the path to be used for storing reports and supporting output,
#' by modifying the path to the tidy data to be found in the associated
#' metadata.
#'
#' @param metadata result of \code{\link{get_tracking_metadata}}
#'
#' @importFrom iidda strip_blob_github
#' @importFrom dplyr %>%
#' @export
make_report_path = function(metadata) {
 (metadata$TidyDataset$path_tidy_data
    %>% strip_blob_github
    %>% sub(pattern = "derived-data", replacement = "supporting-output")
  )
}
