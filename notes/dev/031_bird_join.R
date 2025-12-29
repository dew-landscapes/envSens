library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = use_cores),
  workspace_on_error = TRUE # inspect the error using tar_traceback(target)
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

tar_plan(
  tar_target(name = joined_table,
             command = bird_taxa %>% 
               join_database(ausbird, prefix = "aub_", alt = alt.names) %>% 
               join_database(birdbase %>% get_birdbase(), # because it's messy
                             prefix = "bb_", alt = alt.names) %>% 
               join_database(genlength, prefix = "bl_", alt = alt.names) %>% 
               join_database(eoo, prefix = "bl_", alt = alt.names) %>% 
               dplyr::distinct() %>% 
               select(-contains("match"))
  ),
  
  ## select cols for sensitivity scoring -------
  tar_target(
    name = info_table,
    command = joined_table %>%
      dplyr::select(
        search_term, aub_Taxon_common_name_2, # Names
        aub_National_movement_Total_migrant_13,
        aub_National_movement_Partial_migrant_13,
        bb_DB,
        bb_HB,
        bb_hb_simpson,
        bb_db_simpson,
        bb_RR,
        aub_Feeding_habitat_Agricultural_landscapes_9,
        aub_Feeding_habitat_Urban_landscapes_9,
        aub_Breeding_habitat_Agricultural_lands_9,
        aub_Breeding_habitat_Urban_9,
        bl_Generation_length,
        `bl_Extent_of_Occurrence_(breeding/resident)`
      )
  ),
  
  ## Make table for manual filling -------
  tar_target(
    name = mtable,
    command = make_manual_table(info_table, dir = "bird_db/user")
  )
)