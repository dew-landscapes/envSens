#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ausbird
#' @return
#' @author eryntw
#' @export
#' 
get_ausbird <- function(ausbird = NULL) {
  
  library("traitdata")
  data("australian_birds")
  names(australian_birds) <- gsub("^X\\d+_", "", names(australian_birds))
  aub_trim <- australian_birds %>% 
    dplyr::filter(is.na(Subspecies_name_2)) %>%  # filter subspecies
    dplyr::select(Genus, Species, Taxon_common_name_2, 
                  dplyr::contains("_habitat_"), 
                  dplyr::contains("migrant"), 
                  dplyr::contains("Food_")) %>% 
    dplyr::mutate(
      DB = rowSums(dplyr::select(., dplyr::contains("Food_"))),
      breeding_HB = rowSums(dplyr::select(., dplyr::contains("Breeding_habitat"))),
      feeding_HB = rowSums(dplyr::select(., dplyr::contains("Feeding_habitat"))), 
    )
  return(aub_trim)
}
