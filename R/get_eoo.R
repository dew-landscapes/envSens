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
    select(`Scientific name`, `Extent of Occurrence (breeding/resident)`) %>% 
    separate(`Scientific name`, into = c("Genus", "Species"), sep = " ") %>% 
    mutate(logEOO = `Extent of Occurrence (breeding/resident)` %>% 
             as.numeric() %>% 
             log10()) %>% 
    filter(logEOO > 1) # remove extinct spp
  
}
