.onLoad <- function(lib, pkg) {
  options(

    # list of reference table data frames
    reference_table_list = LBoM.tools:::global_reference_tables,


    all_cats_acm = LBoM.tools:::all_cats_acm,
    all_cats_mort = LBoM.tools:::all_cats_mort,

    original_format = FALSE,
    for_lbom = TRUE
  )
}
