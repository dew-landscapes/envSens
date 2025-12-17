#' Clean and Scale Bird Sensitivity Table
#'
#' This function cleans manually mapped bird sensitivity tables by:
#' (1) replacing NA in selected habitat-use columns with zero,
#' (2) computing log10-transformed range size (EOO) and scaling it 0–1,
#' (3) computing log-transformed generation length and scaling it 0–1.
#'
#' @title Clean mapped bird sensitivity table
#' @param mapped_table A data.frame produced after mapping values into the bird info table.
#'
#' @return A cleaned and scaled data.frame.
#' @author eryntw
#' @export
#'
clean_bird_sensitivity <- function(mapped_table) {
  
  cleaned_table <- mapped_table %>%
    
    # ---- Human-modified habitat usage ----
  mutate(
    aub_Breeding_habitat_Agricultural_lands_9 =
      ifelse(is.na(aub_Breeding_habitat_Agricultural_lands_9),
             0,
             aub_Breeding_habitat_Agricultural_lands_9),
    
    aub_Breeding_habitat_Urban_9 =
      ifelse(is.na(aub_Breeding_habitat_Urban_9),
             0,
             aub_Breeding_habitat_Urban_9)
  ) %>%
    
    # ---- Scale range size (EOO) ----
  mutate(
    bl_logEOO = as.numeric(`bl_Extent_of_Occurrence_(breeding/resident)`) %>% 
      log10(),
    scaled_bl_logEOO = (bl_logEOO - 0.699) / (8.59 - 0.699)
  ) %>%
    
    # ---- Scale generation length ----
  mutate(
    bl_lnGenlength = as.numeric(bl_Generation_length) %>% 
      log(),
    scaled_bl_lnGenLength = (bl_lnGenlength - 0.61) / (3.43 - 0.61)
  )
  
  return(cleaned_table)
}
