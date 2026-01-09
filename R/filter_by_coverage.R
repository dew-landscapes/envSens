#' Filter columns by data coverage
#'
#' Filters out columns in a data frame that do not meet a minimum data coverage
#' threshold. Coverage is defined as the proportion of rows with non-missing and
#' non-empty values.
#'
#' @param df A data frame.
#' @param threshold Numeric between 0 and 1. Columns with coverage lower than this
#'   value will be removed. Default is 0.5.
#'
#' @return A data frame containing only columns with coverage greater than or equal
#'   to the specified threshold.
#'
#' @details
#' Coverage is calculated as:
#' \deqn{(\text{non-NA and non-empty values}) / (\text{total number of rows})}
#'
#' Empty strings (`""`) are treated as missing values.
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, 2, NA, 4),
#'   b = c("", "", "x", ""),
#'   c = c("a", "b", "c", "d")
#' )
#'
#' filter_by_coverage(df, threshold = 0.5)
#'
#' @author eryntw
#' @export
filter_by_coverage <- function(df, threshold = 0.5) {
  
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      threshold < 0 || threshold > 1) {
    stop("`threshold` must be a single numeric value between 0 and 1.")
  }
  
  # Calculate coverage (non-empty, non-NA)
  coverage <- data.frame(
    column = names(df),
    percent = colSums(df != "" & !is.na(df)) / nrow(df),
    stringsAsFactors = FALSE
  )
  
  # Keep only columns with sufficient coverage
  df[, coverage$column[coverage$percent >= threshold], drop = FALSE]
}
