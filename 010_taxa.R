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

tar_plan(
  
  ## Input species list -------
  
  tarchetypes::tar_file_read(name = pia_bp,
                             command = "data/taxa_summary_Braemer Province.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  tarchetypes::tar_file_read(name = pia_usg,
                             command = "data/taxa_summary_Upper Spencer Gulf - Gawler Ranges.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  ## manipulation -------
  
  splist = rbind(pia_bp, pia_usg) %>% 
    organise_piaout()
  ,
  ## write taxa -------
  
  tar_file(name = taxa,
           command = write_with_stamp(splist, "taxa", "taxa/user", ext = "rds")
)

