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
                             command = "H:/data/envSens/database/latest_generation_lengths_of_the_world's_birds_2025.xlsx",
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = 1,
                                                       skip = 1,
                                                       col_types = "guess")
                             #, format = "file"
  ),
  
  tarchetypes::tar_file_read(name = bl_eoo,
                             command = "H:/data/envSens/database/EOO_cobi13486-sup-0003-tables3.csv",
                             read = readr::read_csv(file = !!.x, col_types = readr::cols())
                             #, format = "file"
  ),
  
  # tarchetypes::tar_file_read(name = ala_taxonomy,
  #                            command = "H:/data/taxonomy/galah.parquet",
  #                            read = readr::read_csv(file = !!.x, col_types = readr::cols())
  #                            #, format = "file"
  # ),
  # 
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
  tar_target(name = bird_table,
             command = bird_taxa %>% 
               join_database(aub, prefix = "aub_", alt = alt.names) %>% 
               join_database(bb, prefix = "bb_", alt = alt.names) %>% 
               join_database(genlength, prefix = "bl_", alt = alt.names) %>% 
               join_database(eoo, prefix = "bl_", alt = alt.names) %>% 
               dplyr::distinct() %>% 
               select(-contains("match")))
  
)



