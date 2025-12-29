#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param genlength
#' @return
#' @author eryntw
#' @export
#' 
get_genlength <- function(genlength) {
  
  genlength_trim <- genlength %>%
    tidyr::separate(`Species name 2024`, into = c("Genus", "Species"), sep = " ") %>% 
    dplyr::mutate(lnGenLength = `Generation length` %>% 
             base::log())
  
  return(genlength_trim)
  
}
