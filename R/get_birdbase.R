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
  
  hb_cols <- c("F", "BM", "WD", "SH", "SV", "G", "PL", "R", "D", "A", "C", "RV", "W", "SE", "O")
  db_cols <- birdbase %>% select(contains("Wt"), -SUM.Wt) %>% names()
  
  birdbase_trim <- birdbase %>%
    dplyr::select(Genus, Species, Primary.Diet, DB, HB, RR, db_cols, hb_cols) %>% 
    mutate(across(everything(), ~ replace(., . == "T", "0"))) %>% 
    mutate(across(hb_cols, ~ replace_na(., "0"))) %>% 
    clean_strtypes() %>% 
    bind_cols(
      calc_diversity(., db_cols) %>% rename(db_shannon = shannon, db_simpson = simpson),
      calc_diversity(., hb_cols) %>% rename(hb_shannon = shannon, hb_simpson = simpson)
    )
  return(birdbase)
}
