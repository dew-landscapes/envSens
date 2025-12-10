#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param .
#' @param db_cols
#' @return
#' @author eryntw
#' @export

## This function calculates simpson and shannon biodiversity index for diet and habitat breadth

calc_diversity <- function(df, cols) {
  
  calc_df <- df %>%
    dplyr::mutate(across(where(is.integer), ~ as.numeric(.))) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      total = sum(dplyr::c_across(dplyr::all_of(cols)), na.rm = TRUE),
      p = list(dplyr::c_across(dplyr::all_of(cols)) / total),
      
      # Shannon: -Σ p * ln(p)
      shannon = -sum(p * log(p), na.rm = TRUE),
      
      # Simpson: 1 - Σ p²  (Gini–Simpson)
      simpson = 1 - sum(p^2, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(shannon, simpson)
  
  return(calc_df)
}
