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
                  PrimaryDiet, Db, Hb, Rr, ElevationalRange, Mig, Alt,
                  db_cols, hb_cols) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "T"))) %>% 
    dplyr::mutate(dplyr::across(hb_cols, ~ tidyr::replace_na(., 0))) %>% 
    dplyr::mutate(
      mig_score = dplyr::case_when(
        is.na(Mig) & is.na(Alt)     ~ 0,
        Mig == 1                    ~ 3,
        Mig == 2 | Alt == 1         ~ 2,
        TRUE                        ~ 1
      )
    ) %>% 
  readr::type_convert() %>% 
    dplyr::bind_cols(
      calc_diversity(., db_cols) %>% rename(db_shannon = shannon, db_simpson = simpson)
    )
  return(birdbase_trim)
}
