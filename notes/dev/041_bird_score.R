library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages
               , controller = crew_controller_local(workers = use_cores),
               workspace_on_error = TRUE)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

info_table <- tar_read(info_table, store = "bird_db")

tar_plan(
  
  ## Read manually processed mtable -------

  tar_file_read(name = processed_mtable,
                command = "bird_db/user/mtable.csv",
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## map the data from processed mtable to the info table -------
  
  tar_target(name = mapped_table,
             command =  processed_mtable %>%
               map_by_rowcol(
                 B = info_table,
                 x = "search_term",
                 Atype = "long"
               ) %>%
               clean_bird_sensitivity()),
  
  scored_table = score_bird_sensitivity(mapped_table, outpath = "bird_score/user")
)





