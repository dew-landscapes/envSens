#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mapped
#' @return
#' @author eryntw
#' @export
#' 
score_bird_sensitivity <- function(mapped, outpath) {
  
  scored <- mapped %>% 
    
    ## clean values and set types ------
  
  mutate(across(where(is.character), ~na_if(., "NAV"))) %>% 
    readr::type_convert() %>% 
    
    ## scoring traits ------
  mutate(
    
    ## score migration ------
    score_mig = case_when(
      aub_NationalMovementTotalMigrant13 == 1 ~ 3,
      aub_NationalMovementPartialMigrant13 == 1 ~ 2,
      TRUE ~ 1) %>% 
      (\(x) x / 3)(),
    
    ## score diet breadth ------
    score_DB = 1 - bb_db_simpson,
    
    ## score habitat breadth ------
    score_HB = 1/bb_Hb,
    
    ## score range size ------
    score_rangesize =  1 - bl_eoo_log10Scaled,
    
    ## score generation length ------
    score_genlength = bl_genlen_logScaled,
    
    ## score adaptability to modified env ------
    breedadapt = aub_BreedingHabitatAgriculturalLands9 +
      aub_BreedingHabitatUrban9,
    
    feedadapt = aub_FeedingHabitatAgriculturalLandscapes9 +
      aub_FeedingHabitatUrbanLandscapes9,
    
    score_adapt = case_when(
      (breedadapt+feedadapt) > 2 ~ 0,
      breedadapt > 0 ~ 1,
      feedadapt > 0 ~ 2,
      (breedadapt+feedadapt) == 0 ~ 3) %>% (\(x) x / 3)(),
    
    ## WEIGH range size ------
    score_RistrictRange = bb_Rr, # Restricted range
    
  ) %>%
    
    ## calculate sensitivity scores ------    
  mutate(
    n_cols = ncol(select(., contains("score_"))),
    
    sensitivity_index = rowSums(select(., contains("score_"))),
    
    sensitivity_index_scaled = sensitivity_index / n_cols)
  
  write_csv(scored, file.path(outpath, "scored_bird_sensitivity.csv"))
  return(scored)
  
}
