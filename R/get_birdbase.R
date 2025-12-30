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
  
  hb_cols <- c("F", "Bm", "Wd", "Sh", "Sv", "G", "Pl", "R", 
               "D", "A", "C", "Rv", "W", "Se", "O")
  db_cols <- birdbase %>% 
    dplyr::select(contains("Wt"), -SumWt) %>% 
    base::names()
  
  birdbase_trim <- birdbase %>%
    dplyr::select(Genus, Species, common,
                  PrimaryDiet, Db, Hb, Rr,
                  db_cols, hb_cols) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "T"))) %>% 
    dplyr::mutate(dplyr::across(hb_cols, ~ tidyr::replace_na(., 0))) %>% 
    readr::type_convert() %>% 
    dplyr::bind_cols(
      calc_diversity(., db_cols) %>% rename(db_shannon = shannon, db_simpson = simpson)
    )
  return(birdbase_trim)
}
