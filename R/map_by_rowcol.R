#### map_by_rowcol ####
## Map values from A to B by matching row names and specified column names.
## Arguments:
## A: data frame to map from (e.g., mtable)
## B: data frame to map to (e.g., info table)
## x: reference column (e.g., "search_term")
## Atype: if "long", pivot_wider the table; otherwise ignored
## overwrite: logical, whether to overwrite existing values in B

#' Map values from one table to another by matching rows and columns
#'
#' This function maps values from data frame `A` to data frame `B` by matching
#' the reference column `x` (e.g., species names) and only the columns common to both.
#' It can optionally pivot `A` from long to wide format before mapping.
#'
#' @param A data frame to map values from
#' @param B data frame to map values to
#' @param x character, reference column name existing in both A and B
#' @param Atype character, if "long", `A` is pivoted from long to wide
#' @param overwrite logical, whether to overwrite existing values in B (default FALSE)
#'
#' @return A data frame B with mapped values from A
#' @author eryntw
#' @export
#'
#' @examples
#' # Example with long table
#' # map_by_rowcol(A = tbl_long, B = info_table, x = "search_term", Atype = "long")
#'
#' # Example without pivoting
#' # map_by_rowcol(A = tbl_wide, B = info_table, x = "search_term", overwrite = TRUE)
#' 
map_by_rowcol <- function(A, B, x, Atype = NULL, overwrite = FALSE) {
  
  # Pivot A if needed
  if (!is.null(Atype) && Atype == "long") {
    A <- tidyr::pivot_wider(A, names_from = "trait", values_from = "value")
  }
  
  # Check reference column exists
  if (!x %in% base::names(A) || !x %in% base::names(B)) {
    stop("Reference column x must exist in both A and B.")
  }
  
  # Determine columns to map (must exist in both A and B)
  map_cols <- base::intersect(base::names(A), base::names(B))
  map_cols <- setdiff(map_cols, x)  # exclude reference column itself
  if (length(map_cols) == 0) stop("No columns exist in BOTH A and B to map.")
  
  # Identify matching rows by reference column x
  common_vals <- base::intersect(A[[x]], B[[x]])
  if (length(common_vals) == 0) stop("No matching values in reference column x.")
  
  # Pull vectors from A for matching rows
  a_vals <- A[A[[x]] %in% common_vals, , drop = FALSE]
  
  # Define missing values in B
  is_missing <- c(NA, "", -999, "NAV")
  
  # Loop over columns and matching rows
  for (col in map_cols) {
    for (val in common_vals) {
      rows_B <- B[[x]] == val
      rows_A <- a_vals[[x]] == val
      
      if (overwrite) {
        B[rows_B, col] <- a_vals[rows_A, col]
      } else {
        # Only replace missing values
        missing_idx <- rows_B & B[[col]] %in% is_missing
        B[missing_idx, col] <- a_vals[rows_A, col]
      }
    }
  }
  
  return(B)
}

