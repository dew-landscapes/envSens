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

info_table <- tar_read(info_table, store = tars$bird_mtable$store)
joined_table <- tar_read(joined_table, store = tars$bird_db$store)

tar_plan(
  
  ## Read manually processed mtable -------
  
  tar_file_read(name = processed_mtable,
                command = "data/current_mtable.csv",
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## Scaling range, generation length and elevation using all AU birds ------
  
  tar_target(name = scaled_infotable,
             command = map_by_rowcol(A = processed_mtable, 
                                     B = joined_table %>% 
                                       dplyr::select(dplyr::any_of(names(info_table))),
                                     x = "search_term", 
                                     Atype = "long") %>% 
               dplyr::mutate(
                 bl_eoo_log10Scaled = scales::rescale(log10(bl_RlEooSmallerOfBreedingAndNonBreedingEoo),
                                                      to = c(0, 1), na.rm = TRUE),
                 bl_genlen_logScaled = scales::rescale(log(bl_GenerationLength),
                                                       to = c(0, 1), na.rm = TRUE),
                 bl_log10ElevScaled = scales::rescale(log10(`rec_dem-9s_range_90_10_norm`),
                                                      to = c(0, 1), na.rm = TRUE)
               ) %>% 
               dplyr::inner_join(info_table %>% dplyr::select("search_term", "common"),
                                 by = "search_term") %>% 
               score_ag_urb_habitats()
             
  ),
  
  ## Final score table ------
  
  scored_table_v1 = birdsens_v1_original(scaled_infotable,
                                         outpath = tars$bird_score$store,
                                         return = "scored"),
  
  scored_table_v2 = birdsens_v2_poolall(scaled_infotable,
                                        outpath = tars$bird_score$store,
                                        return = "scored"),
  
  scored_table_v3 = birdsens_v3_mixed(scaled_infotable,
                                      outpath = tars$bird_score$store,
                                      return = "scored")
)





