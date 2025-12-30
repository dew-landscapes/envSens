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

joined_table <- tar_read(joined_table, store = tars$bird_db$store)

tar_plan(
  
  ## select cols for sensitivity scoring -------
  tar_target(
    info_table,
    joined_table %>%
      dplyr::select(
        search_term, common, # Names
        aub_NationalMovementTotalMigrant13,
        aub_NationalMovementPartialMigrant13,
        bb_Db,
        bb_Hb,
        bb_db_simpson,
        bb_Rr,
        aub_FeedingHabitatAgriculturalLandscapes9,
        aub_FeedingHabitatUrbanLandscapes9,
        aub_BreedingHabitatAgriculturalLands9,
        aub_BreedingHabitatUrban9,
        bl_GenerationLength,
        bl_ExtentOfOccurrenceBreedingResident
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("Urban|Agricul"),
          ~ tidyr::replace_na(.x, 0L)
        )
      )
    ## Replace NA with 0 in urban and agriculture breeders;
    ## These are migratory water birds
  ),
  
  
  ## Make table for manual filling -------
  tar_target(
    name = mtable,
    command = make_manual_table(info_table, dir = "data")
  )
)