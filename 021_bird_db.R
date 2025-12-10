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

taxa <- tar_read(splist, store = "taxa")

tar_plan(
  
  ## Static database -------
  
  tarchetypes::tar_file_read(name = birdbase,
                             command = "H:/data/envSens/database/data_BIRDBASE v2025.1 Sekercioglu et al. Final.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  tarchetypes::tar_file_read(name = bl_genlength,
                             command = "H:/data/envSens/database/Modelled_GenLength_cobi13486-sup-0004-tables4.csv",
                             read = readr::read_excel(file = !!.x, 
                                                      sheet = 1,
                                                      skip = 1,
                                                      col_types = readr::cols())
                             #, format = "file"
  ),
  
  tarchetypes::tar_file_read(name = bl_eoo,
                             command = "H:/data/envSens/database/EOO_cobi13486-sup-0003-tables3.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  ## PIA taxa -------
  tar_target(name = bird_taxa,
             command = taxa %>% 
               dplyr::filter(ala_class == "Aves"))
  ,
  ## Birdbase trim -------
  bb = get_birdbase(birdbase)
  ,
  ## Ausbird trim -------
  aub = get_ausbird(ausbird)
  ,
  ## Genlength trim -------
  genlength = get_genlength(bl_genlength)
  ,
  ## EOO trim -------
  eoo = get_eoo(bl_eoo)
  ,
  ## Combined database -------
  bird_table <- bird_taxa %>% 
    join_database(aub, prefix = "aub_") %>% 
    join_database(bb, prefix = "bb_") %>% 
    join_database(genlength, prefix = "bl_") %>% 
    join_database(eoo, prefix = "bl_") %>% 
    dplyr::distinct() %>% 
    select(-contains("match"))
)



