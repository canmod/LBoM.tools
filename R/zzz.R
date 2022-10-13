.onLoad <- function(lib, pkg) {
  options(

    # list of reference table data frames
    reference_table_list = LBoM.tools:::global_reference_tables,


    all_cats = LBoM.tools:::all_cats,

    original_format = FALSE,
    for_lbom = TRUE
  )
}
