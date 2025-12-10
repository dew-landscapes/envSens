#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param eoo
#' @return
#' @author eryntw
#' @export
#' 
get_eoo <- function(eoo) {
  
  eoo_trim <- eoo %>% 
    dplyr::select(`Scientific name`, `Extent of Occurrence (breeding/resident)`) %>% 
    tidyr::separate(`Scientific name`, into = c("Genus", "Species"), sep = " ") %>% 
    dplyr::mutate(logEOO = `Extent of Occurrence (breeding/resident)` %>% 
             as.numeric() %>% 
             log10()) %>% 
    dplyr::filter(logEOO > 1) # remove extinct spp
  
}
