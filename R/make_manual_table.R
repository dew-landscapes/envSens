#' Update and Extend a Manually Curated Table
#'
#' This function extracts missing rows from a new dataset, appends them to an 
#' existing manually curated table, and writes an updated combined table for 
#' further manual completion.
#'
#' @details
#' Maintains a consistent workflow for manually curated tables (e.g., sensitivity 
#' tables such as `bird_table_sensitivity`). Performs three key tasks:
#' 1. Identifies rows in `df_new` that contain missing values in the selected columns.
#' 2. Appends only new unique rows to the existing table, which is 
#'    provided in the directory.
#' 3. Writes the updated combined table to a CSV file to allow further manual editing.
#'
#' @title Update manual table with new missing-value rows
#'
#' @param df_new A data frame containing the new set of records to check.
#' @param dir Directory path where the existing `mtable.csv` is stored (or will be created).
#' @param overwrite Logical; if TRUE, overwrite the existing `mtable.csv`; if FALSE, 
#'        create a new CSV with a timestamp using `write_with_stamp()`.
#'
#' @return A data frame containing the combined old + newly added rows.
#'
#' @author eryntw
#' @export

make_manual_table <- function(df_new, dir, save_previous = FALSE) {
  
  # --- 1. Create long table of missing values ---
  cols <- setdiff(names(df_new), c("search_term", "aub_Taxon_common_name_2"))
  
  tbl <- df_new %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols), as.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols),
      names_to = "trait",
      values_to = "value"
    ) %>%
    dplyr::filter(is.na(value) | value %in% c("-999", "NAV")) %>%
    dplyr::distinct()
  
  # --- 2. Path to processed manual table ---
  mtable_path <- file.path(dir, "current_mtable.csv")
  
  # --- 3. Check if mtable exists ---
  if (!file.exists(mtable_path)) {
    csv <- tbl
    message("No existing mtable found. Creating new table.")
  } else {
    # --- 3.1 Read existing table ---
    processed <- readr::read_csv(mtable_path, col_types = readr::cols())
    
    # --- 3.2 Identify new rows to add ---
    new_rows <- tbl %>%
      dplyr::select(-value) %>%
      dplyr::anti_join(processed %>% dplyr::select(-value), by = c("search_term", "trait")) %>%
      dplyr::mutate(value = NA)
    
    # --- 4. Combine existing and new rows ---
    csv <- dplyr::bind_rows(processed, new_rows)
    message(paste0("Found ", nrow(new_rows), " new rows to append."))
  }
  
  # --- 5. Write CSV ---
  if (save_previous) {
    readr::write_csv(csv, mtable_path)
    write_with_stamp(processed, "outdated_mtable", dir, ext = "csv")
  } else {
    readr::write_csv(csv, mtable_path)
  }
  
  return(csv)
}
