#' Replace Taxa Names Using a Lookup Table
#'
#' Standardises taxonomic names in a data frame by replacing values
#' in a specified taxa column using a predefined lookup table.
#' Any taxa not found in the lookup table are left unchanged.
#'
#' @param df A data frame containing a taxonomic column to be standardised.
#' @param taxa_col Character. Name of the column in \code{df} containing taxa
#'   names to be checked and replaced (default: "search_term").
#'
#' @return The original data frame with updated taxonomic names in
#'   \code{taxa_col}.
#'
#' @details
#' This function uses an internal named character vector as a lookup table,
#' where names represent original taxa names and values represent the
#' standardised replacements.
#'
#' Only exact matches are replaced. Taxa not listed in the lookup
#' table remain unchanged.
#'
#' @examples
#' df <- data.frame(search_term = c("Ardea intermedia", "Ardea alba"))
#'
#' replace_taxa(df)
#'
#' @importFrom dplyr if_else
#' @export
replace_taxa <- function(df, taxa_col = "search_term") {
  
  # ===============================
  # Taxa replacement lookup table
  # ===============================
  
  lookup <- c(
    "Ardea intermedia" = "Ardea plumifera"
  )
  
  # ===============================
  # Input validation
  # ===============================
  
  stopifnot(
    is.data.frame(df),
    taxa_col %in% names(df),
    is.character(lookup),
    !is.null(names(lookup))
  )
  
  # ===============================
  # Replace taxa names
  # ===============================
  
  df[[taxa_col]] <- dplyr::if_else(
    df[[taxa_col]] %in% names(lookup),
    lookup[df[[taxa_col]]],
    df[[taxa_col]]
  )
  
  df
}
