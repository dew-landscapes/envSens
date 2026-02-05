#' Min–max normalise numeric columns with optional log transform
#'
#' This function normalises numeric columns using min–max scaling.
#' If the maximum value of a column exceeds a specified threshold,
#' values are log-transformed (log1p) prior to scaling.
#' All resulting values are rounded to a fixed number of decimal places.
#'
#' @param df A data frame or tibble.
#' @param cols Character vector of column names to normalise.
#' @param log_threshold Numeric. Apply log1p() if max > threshold (default = 100).
#' @param digits Integer. Number of decimal places to round to (default = 2).
#' @param suffix Suffix appended to normalised columns (default = "_norm").
#'
#' @return A tibble with additional normalised columns.
#'
#' @examples
#' df_norm <- normalise_minmax(
#'   df,
#'   cols = c("ADI", "TXX", "PTX")
#' )
#'
#' @export
normalise_minmax <- function(
    df,
    cols,
    log_threshold = 100,
    digits = 2,
    suffix = "_norm"
) {
  
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(cols),
        ~ {
          x <- .x
          x_non_na <- x[!is.na(x)]
          
          # All NA or constant → return NA
          if (length(x_non_na) == 0 || length(unique(x_non_na)) == 1) {
            return(rep(NA_real_, length(x)))
          }
          
          # Conditional log transform
          if (max(x_non_na) > log_threshold) {
            x <- log1p(x)
          }
          
          # Min–max scaling
          x_scaled <-
            (x - min(x, na.rm = TRUE)) /
            (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
          
          # Round
          round(x_scaled, digits)
        },
        .names = "{.col}{suffix}"
      )
    )
}
