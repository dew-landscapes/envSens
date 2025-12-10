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
  
  genlength_trim <- genlenth %>%
    separate(`Species name 2024`, into = c("Genus", "Species"), sep = " ") %>% 
    mutate(lnGenLength = as.numeric(GenLength) %>% log())
  
  return(genlength_trim)
  
}
