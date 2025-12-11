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
      aub_National_movement_Total_migrant_13 == 1   ~ 3,
      aub_National_movement_Partial_migrant_13 == 1 ~ 2,
      TRUE                                          ~ 1) %>% 
      (\(x) x / 3)(),
    
    ## score diet breadth ------
    score_DB = 1 - bb_db_simpson,
    
    ## score habitat breadth ------
    score_HB = 1 - bb_hb_simpson,
    
    ## score range size ------
    scale_rangesize =  1 - scaled_bl_logEOO,
    
    ## score generation length ------
    scale_genlength = scaled_bl_lnGenLength,
    
    ## score adaptability to modified env ------
    breedadapt = aub_Breeding_habitat_Agricultural_lands_9 +
      aub_Breeding_habitat_Urban_9,
    
    feedadapt = aub_Feeding_habitat_Agricultural_landscapes_9 +
      aub_Feeding_habitat_Urban_landscapes_9,
    
    score_adapt = case_when(
      (breedadapt+feedadapt) > 2 ~ 0,
      breedadapt > 0 ~ 1,
      feedadapt > 0 ~ 2,
      (breedadapt+feedadapt) == 0 ~ 3) %>% (\(x) x / 3)(),
    
    ## WEIGH range size ------
    score_RistrictRange = bb_RR, # Restricted range
    
    ) %>%
    
    ## calculate sensitivity scores ------    
    mutate(
      n_cols = ncol(select(., 
                           contains("score"), 
                           contains("scale"))),
      
      sensitivity_index = rowSums(select(., 
                                         contains("score"), 
                                         contains("scale"))),
      
      sensitivity_index_scaled = sensitivity_index / n_cols)
  
  write_csv(scored, file.path(outpath, "scored_bird_sensitivity.csv"))
  return(scored)
  
}
