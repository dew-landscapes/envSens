#### organise_piaout ####

## This function organise the final species list output from PIA. Note that this function is highly dependent on the column names of each data sources (ALA, PIA outputs)

organise_piaout <- function(df) {
  
  # 1. label subspecies with 1/0, add a column of listing hierarchy, add prefix pia_
  final <- df %>%   # Remove overlapping species
    dplyr::select(taxa, contains("name"), contains("status")) %>% 
    dplyr::distinct() %>%  # Remove duplicates due to different models
    dplyr::mutate(
      subspecies = dplyr::case_when(   # Label subspecies
        stringr::str_count(npw_listed_name, "\\S+") > 2 |
          stringr::str_count(future_listed_name, "\\S+") > 2 ~ 1L,
        TRUE ~ 0L
      ),
      listing_hierarchy = dplyr::case_when(   # Label listing hierarchy
        epbc_status != "" & !is.na(epbc_status) ~ "epbc",
        npw_status != "" & !is.na(npw_status) ~ "npw",
        future_status != "" & !is.na(future_status) ~ "future",
        TRUE ~ "reg_cont"
      )
    )
  
  final <- final %>% 
    dplyr::rename_with(~ base::paste0("pia_", .x), base::names(final[-1]))
  
  # 3. Extract class columns from ALA
  ala <- galah::search_taxa(final$taxa) %>%
    dplyr::distinct() %>% 
    dplyr::select(search_term, kingdom, class, family, vernacular_name, match_type)
  
  ala <- ala %>% 
    dplyr::rename_with(~ base::paste0("ala_", .x), base::names(ala[-1]))
  
  # 4. ALA join final
  splist <- final %>%
    dplyr::left_join(ala, by = join_by(taxa == search_term)) %>% 
    dplyr::rename(search_term = taxa)
  
  # 5. Filter out invert and non-vascular plants that are from regional contribution and split the dataset to major groups by class
  class_list <- c("Equisetopsida", "Aves", "Mammalia", "Reptilia", "Actinopterygii", "Amphibia")
  
  splist <- splist %>%
    dplyr::filter(pia_listing_hierarchy != "reg_cont" | ala_class %in% class_list)
  
  return(splist)
}

## This function add the date and time stamp to the file name that will be saved as rds.

write_with_stamp <- function(object, name, dir) {
  
  # time stamp e.g., 2025-12-10_14-30
  stamp <- base::format(Sys.time(), "%Y-%m-%d_%H-%M")
  
  # path
  path <- base::file.path(base::paste0(dir, "/", name, "_", stamp, ".rds"))

  # write
  base::saveRDS(object, file = path)
  
  message("Saved")
}
