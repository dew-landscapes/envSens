#' Scale a trait relative to min and max of all Australian birds
#'
#' This function takes a continuous trait of a species and scales it between 0 and 1
#' based on the minimum and maximum values of that trait across all Australian birds.
#' Useful for normalizing trait values for comparative analysis.
#'
#' @title Scale Australian Bird Trait Min-Max
#' @param sp_col Numeric vector of trait values for species to scale.
#' @param df A data.frame of species with trait columns (must include column `df_col`).
#' @param df_col Character string of the column name in `df` to use for scaling.
#' @return A list with two elements:
#'   - `col`: Numeric vector of scaled values between 0 and 1.
#'   - `minmax`: data.frame with the min and max values used for scaling.
#' @author eryntw
#' @export
#'
scale_aubird_minmax <- function(sp_col, df, df_col) {
  
  # Convert column name to symbol for tidy evaluation
  col_sym <- rlang::sym(paste0("minmax_", df_col))
  
  # Compute min and max across all Australian birds
  value <- dplyr::distinct(df %>%
                             dplyr::mutate(search_term = paste(Genus, Species)) %>%
                             dplyr::select(search_term)) %>%
    join_database(df, prefix = "minmax_", alt = alt.names) %>%
    dplyr::summarise(
      min = min(!!col_sym, na.rm = TRUE),
      max = max(!!col_sym, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Scale the input species column
  scaled_col <- (sp_col - value$min) / (value$max - value$min)
  
  # Return scaled column and min/max values
  return(list(col = scaled_col, minmax = value))
}
