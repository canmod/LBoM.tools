library(LBoM)
library(dplyr)
library(iidda)

all_cats = (LBoM::masterList$acm
   %>% lapply(names)
   %>% unlist
   %>% tolower
   %>% unique
)

pipeline_dir = "../LBoM-data/derived-data-pipeline"
ref_tab_paths = list.files(iidda::sprintf_named(
  "%{pipe_dir}s/%{file_types}s/reference/",
  pipe_dir = pipeline_dir,
  file_types = list.files(pipeline_dir)
), full.names = TRUE)

ref_tabs = sapply(ref_tab_paths, read.csv, simplify = FALSE)

global_reference_tables = (ref_tabs
  %>% split(tools::file_path_sans_ext(basename(names(ref_tabs))))
  %>% sapply(dplyr::bind_rows, simplify = FALSE)
  %>% sapply(unique, simplify = FALSE)
)

save(list = c("all_cats", "global_reference_tables"), file = "R/sysdata.rda")
