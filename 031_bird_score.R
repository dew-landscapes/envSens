library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages
               , controller = crew_controller_local(workers = use_cores))

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

sensitivity <- tar_read(bird_table_sensitivity, store = "bird_db")

tar_plan(
  
  ## Read manually processed mtable -------
  
  files = list.files(path = "bird_db/user", 
                     pattern = "\\.csv$",
                     full.names = TRUE),
  
  latest = files[order(files, decreasing = TRUE)][1],
  
  tar_file_read(name = processed_mtable,
                command = latest,
                read = readr::read_csv(latest, col_types = readr::cols())
  ),
  
  ## map the data from processed mtable to the info table -------
  
  mapped = map_by_rowcol(A = processed_mtable,
                         B = sensitivity,
                         x = "search_term", 
                         Atype = "long"),
  
  scored = score_bird_sensitivity(mapped, outpath = "bird_score/user")
)





