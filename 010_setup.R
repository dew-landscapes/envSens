library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages
               , controller = crew_controller_local(workers = use_cores))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# setup -------

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
  
  ## Static database -------
  
  tarchetypes::tar_file_read(name = birdbase,
                             command = "H:/data/envSens/database/data_BIRDBASE v2025.1 Sekercioglu et al. Final.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  tarchetypes::tar_file_read(name = genlength,
                             command = "H:/data/envSens/database/Modelled_GenLength_cobi13486-sup-0004-tables4.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  tarchetypes::tar_file_read(name = eoo,
                             command = "H:/data/envSens/database/EOO_cobi13486-sup-0003-tables3.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  )
)

