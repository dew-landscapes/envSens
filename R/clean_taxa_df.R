#' Clean and standardise a taxonomic data frame
#'
#' Ensures consistent Genus–Species structure, optional common names,
#' removes duplicates, and fills missing common names with "NoName".
#'
#' @param df A data frame containing taxonomic information
#' @param commoncol Optional. Column containing common names (unquoted). Default = NULL
#' @param taxacol Optional. Column containing binomial names "Genus Species" (unquoted). Default = NULL
#'
#' @return A cleaned data frame with columns `Genus`, `Species`,
#' and (if provided) `common`
#' @export
#'
#' @examples
#' clean_taxa_df(df, commoncol = common, taxacol = taxa)
#' clean_taxa_df(df, taxacol = taxa)
#' clean_taxa_df(df)
#'
clean_taxa_df <- function(df, taxacol = NULL, commoncol = NULL) {
  
  out <- df
  commoncol <- dplyr::enquo(commoncol)
  taxacol   <- dplyr::enquo(taxacol)
  
  ## ---- Split taxa column if provided ----
  if (!rlang::quo_is_null(taxacol)) {
    out <- out %>%
      tidyr::separate(
        !!taxacol,
        into = c("Genus", "Species"),
        sep = " ",
        remove = FALSE,
        fill = "right"
      ) %>% 
      dplyr::filter(!is.na(Species))
  }
  
  ## ---- Handle common name column if provided ----
  if (!rlang::quo_is_null(commoncol)) {
    out <- out %>%
      dplyr::rename(common = !!commoncol) %>%
      dplyr::mutate(common = dplyr::coalesce(common, "NoName")) %>%
      dplyr::arrange(Genus, Species, is.na(common)) %>%
      dplyr::distinct(Genus, Species, common, .keep_all = TRUE)
  } else {
    out <- out %>%
      dplyr::arrange(Genus, Species) %>%
      dplyr::distinct(Genus, Species, .keep_all = TRUE)
  }
  
  return(out)
}
