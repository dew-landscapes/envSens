#' Map values from one table to another by matching rows and columns
#'
#' This function maps values from data frame `A` to data frame `B` by matching
#' the reference column `x` (e.g., species names). By default, only the columns 
#' common to BOTH `A` and `B` are used. If `keepcols = TRUE`, the output will 
#' include ALL columns found in either `A` or `B` (union of columns), and missing 
#' values will be filled with NA.
#'
#' If `A` is in long format and `Atype = "long"` is provided, the table will be
#' pivoted to wide format before mapping.
#'
#' @param A data frame to map values FROM
#' @param B data frame to map values TO
#' @param x character, reference column name present in both A and B
#' @param namecase optional, ignored (reserved for future use)
#' @param Atype character, `"long"` to pivot A wider; otherwise no pivoting; assuming WIDE format.
#' @param overwrite logical, whether to overwrite existing values in B (default FALSE)
#' @param keepcols logical, whether to keep ALL unique columns from A and B (default FALSE)
#'
#' @return Updated version of B with mapped values from A
#' @author eryntw
#' @export
#'
#' @examples
#' ### ---------------------------
#' ### Example 1: Simple mapping (default keepcols = FALSE)
#' ### ---------------------------
#' A <- data.frame(
#'   species = c("A","B","C"),
#'   trait1 = c(1,2,3),
#'   trait2 = c(10,20,30)
#' )
#'
#' B <- data.frame(
#'   species = c("A","B","D"),
#'   trait1 = c(NA,5,9),
#'   trait2 = c(NA,NA,99)
#' )
#'
#' map_by_rowcol(A, B, x = "species")
#'
#' ### Expected result:
#' # species trait1 trait2
#' # A        1      10
#' # B        5      20
#' # D        9      99   (no match; unchanged)
#'
#'
#' ### ---------------------------
#' ### Example 2: keepcols = TRUE (retain ALL columns from A and B)
#' ### ---------------------------
#' A <- data.frame(
#'   species = c("A","B"),
#'   new_trait = c(100, 200)
#' )
#'
#' B <- data.frame(
#'   species = c("A","B","C"),
#'   old_trait = c(5, NA, 9)
#' )
#'
#' map_by_rowcol(A, B, x = "species", keepcols = TRUE)
#'
#' ### Expected result:
#' # species old_trait new_trait
#' # A        5        100
#' # B        NA       200
#' # C        9        NA
#'
#'
#' ### ---------------------------
#' ### Example 3: A in long format
#' ### ---------------------------
#' A_long <- data.frame(
#'   species = c("A","A","B","B"),
#'   trait = c("t1","t2","t1","t2"),
#'   value = c(10, 20, 30, 40)
#' )
#'
#' B <- data.frame(
#'   species = c("A","B"),
#'   t1 = c(NA, NA),
#'   t2 = c(0, 0)
#' )
#'
#' map_by_rowcol(A_long, B, x = "species", Atype = "long")
#'
map_by_rowcol <- function(A, B, x,
                          namecase = "upper_camel",
                          Atype = NULL,
                          overwrite = FALSE,
                          keepcols = FALSE) {
  
  # Pivot A from long format
  if (!is.null(Atype) && Atype == "long") {
    A <- tidyr::pivot_wider(A, names_from = "trait", values_from = "value") %>% 
      readr::type_convert()
  }
  
  # Reference column check
  if (!x %in% names(A) || !x %in% names(B)) {
    stop("Reference column x must exist in both A and B.")
  }
  
  # Columns to map
  if (keepcols) {
    # full union set
    all_cols <- union(names(A), names(B))
    # ensure all columns exist in B
    for (col in setdiff(all_cols, names(B))) {
      B[[col]] <- NA
    }
    # ensure all columns exist in A
    for (col in setdiff(all_cols, names(A))) {
      A[[col]] <- NA
    }
    map_cols <- setdiff(all_cols, x)
  } else {
    # only intersection of columns
    map_cols <- intersect(names(A), names(B))
    map_cols <- setdiff(map_cols, x)
    if (length(map_cols) == 0) stop("No columns exist in BOTH A and B to map.")
  }
  
  # Matching rows
  common_vals <- intersect(A[[x]], B[[x]])
  if (length(common_vals) == 0) stop("No matching values in reference column x.")
  
  A_sub <- A[A[[x]] %in% common_vals, , drop = FALSE]
  
  is_missing <- c(NA, "", -999, "NAV")
  
  # Mapping loop
  for (col in map_cols) {
    for (val in common_vals) {
      rows_B <- B[[x]] == val
      rows_A <- A_sub[[x]] == val
      
      if (overwrite) {
        B[rows_B, col] <- A_sub[rows_A, col]
      } else {
        missing_idx <- rows_B & (B[[col]] %in% is_missing)
        B[missing_idx, col] <- A_sub[rows_A, col]
      }
    }
  }
  
  return(B)
}
