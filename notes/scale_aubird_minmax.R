#' Scale a trait relative to min and max of all Australian birds
#'
#' This function reads a CSV file, keeps only rows whose species are Australian birds,
#' extracts a numeric trait column, and returns its min and max for scaling.
#'
#' @title Scale Australian Bird Trait Min–Max
#' @param path File path to a CSV table.
#' @param taxa_col Character. Column name in the CSV representing the taxon (e.g. "Scientific name").
#' @param scale_col Character. Column name in the CSV representing a continuous trait.
#'
#' @return A tibble with two columns: `min` and `max`.
#'
#' @author eryntw
#' @export
#'
#' @examples
#' \dontrun{
#' scale_aubird_minmax(
#'   path = "H:/data/envSens/database/EOO_cobi13486.csv",
#'   taxa_col = "Scientific name",
#'   scale_col = "Extent of Occurrence (breeding/resident)"
#' )
#' }
scale_aubird_minmax <- function(path, taxa_col, scale_col) {
  
  # --- 1. All Australian bird names ---
  aubirds <- traitdata::australian_birds %>% 
    dplyr::mutate(sciname = paste(Genus, Species)) %>% 
    dplyr::distinct(sciname)
  
  # --- 2. Read file and filter to Australian birds ---
  df <- readr::read_csv(path, col_types = readr::cols()) %>% 
    dplyr::filter(.data[[taxa_col]] %in% aubirds$sciname) %>% 
    dplyr::select(.data[[scale_col]])
  
  # --- 3. Compute min and max ---
  value <- df %>% 
    dplyr::summarise(
      min = min(.data[[scale_col]], na.rm = TRUE),
      max = max(.data[[scale_col]], na.rm = TRUE),
      .groups = "drop"
    )
  
  return(value)
}

