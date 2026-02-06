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

pilot_subset <- tar_read(pilot_subset, store = tars$bird_db$store)

tar_plan(
  
  ## select cols for sensitivity scoring -------
  tar_target(
    name = info_table,
    command = 
      pilot_subset %>%
      dplyr::select(
        search_term, common, # Names
        bl_Family, # ID raptors
        aub_BreedingHabitatAgriculturalLands9,
        aub_BreedingHabitatUrban9,
        aub_FeedingHabitatAgriculturalLandscapes9,
        aub_FeedingHabitatUrbanLandscapes9,
        aub_Migratory6,
        bb_Db,
        bb_db_simpson,
        bb_Rr,
        bb_Hb,
        bb_ElevationalRange,
        bb_mig_score,
        bl_MigratoryStatus,
        bl_GenerationLength,
        bl_RlEooSmallerOfBreedingAndNonBreedingEoo,
        bl_scaledHB_L1,
        bl_logscaledHBscore_L2,
        bl_anthro_LogHabitat_scaled,
        rec_stern_dehoedt_2000_minor_simpson,
        rec_geom_90M_s10e110_simpson, 
        `rec_dem-9s_range_90_10_norm`
      )
  ),
  
  
  ## Make table for manual filling -------
  tar_target(
    name = mtable,
    command = make_manual_table(info_table, dir = "data")
  )
)