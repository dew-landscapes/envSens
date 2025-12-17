#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param birdbase
#' @return
#' @author eryntw
#' @export
#' 
get_birdbase <- function(birdbase) {
  
  hb_cols <- c("F", "BM", "WD", "SH", "SV", "G", "PL", "R", 
               "D", "A", "C", "RV", "W", "SE", "O")
  db_cols <- birdbase %>% 
    dplyr::select(contains("Wt"), -`SUM-Wt`) %>% 
    base::names()
  
  birdbase_trim <- birdbase %>%
    dplyr::rename_with(~ gsub(" ", "_", .x)) %>% 
    dplyr::select(Genus, Species, Primary_Diet, DB, HB, RR, db_cols, hb_cols) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "T"))) %>% 
    dplyr::mutate(dplyr::across(hb_cols, ~ tidyr::replace_na(., 0))) %>% 
    readr::type_convert() %>% 
    dplyr::bind_cols(
      calc_diversity(., db_cols) %>% rename(db_shannon = shannon, db_simpson = simpson),
      calc_diversity(., hb_cols) %>% rename(hb_shannon = shannon, hb_simpson = simpson)
    )
  return(birdbase_trim)
}
