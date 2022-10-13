# # get fields names from tidyxl:xlsx_cells output
# # adaptation for all_disease_names for mortality data
# get_all_fields <- function(masterList){
#   (masterList$acm
#    %>% lapply(names)
#    %>% unlist
#    %>% tolower
#    %>% unique
#   )}
#
# # get the names of all 'diseases'
# # (includes information about age ranges,
# # so i prefer to call them categories or cats).
# all_cats = get_all_fields(masterList)  # TODO: some data types use all_disease_names

# the following were built specifically for the mortality
# data and may need to be generalized for additional data
# sets

## TODO: these age re-templates are different for different data types ... ugh

# need to separate disease and age information from the category names.
# so we need to use regular expressions. because we need to use similar
# regular expressions for different purposes, we create templates of
# regular expressions that can be modified for different purposes. the
# following list contains regular expression templates for matching
# different ways to encode age information in the category names.


get_re_templates = function(type) {
  switch(type,
    mortality = list(
      .to. = "%{lower_left}s[0-9]+%{lower_right}s\\.to\\.%{upper_left}s[0-9]+%{upper_right}s(?:\\.years)?",
      .to = "%{lower_left}s[0-9]+%{lower_right}s\\.to%{upper_left}s[0-9]+%{upper_right}s",
      .plus = "%{lower_left}s[0-9]+%{lower_right}s\\.plus",
      less.than. = "less\\.than\\.%{upper_left}s[0-9]+%{upper_right}s",
      .under. = "(?:\\.)?under\\.%{upper_left}s[0-9]+%{upper_right}s(?:\\.year(?:s?))?",
      .all.ages = "(?:\\.)?all\\.ages",
      .and.upwards = "%{lower_left}s[0-9]+%{lower_right}s\\.and\\.upwards"
    ),
    all_cause = list(
      .to. = "(?:\\.)?%{lower_left}s[0-9]+%{lower_right}s\\.to\\.%{upper_left}s[0-9]+%{upper_right}s(?:\\.years)?",
      .to = "(?:\\.)?%{lower_left}s[0-9]+%{lower_right}s\\.to%{upper_left}s[0-9]+%{upper_right}s",
      .plus = "(?:\\.)?%{lower_left}s[0-9]+%{lower_right}s\\.plus",
      less.than. = "less\\.than\\.%{upper_left}s[0-9]+%{upper_right}s",
      .under. = "(?:\\.)?under\\.%{upper_left}s[0-9]+%{upper_right}s(?:\\.year(?:s?))?",
      .all.ages = "(?:\\.)?all\\.ages",
      .and.upwards = "%{lower_left}s[0-9]+%{lower_right}s\\.and\\.upwards",
      single_year = "(?:\\.)?%{left}s[0-9]+%{right}s$"
    )
  )
}

# because the dataset is big and there are many types of bounds
# to look up, we use these functions to create lookup functions
# for extracting upper and lower bounds of age ranges.


get_upper_lookup = function(type) {
  iidda::make_age_hash_table(
    LBoM.tools:::all_cats,
    get_re_templates(type),
    'upper'
  )
}

get_lower_lookup = function(type) {
  iidda::make_age_hash_table(
    LBoM.tools:::all_cats,
    get_re_templates(type),
    'lower'
  )
}
