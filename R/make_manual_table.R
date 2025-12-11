#' Update and Extend a Manually Curated Table
#'
#' This function extracts missing rows from a new dataset, appends them to an 
#' existing manually curated table, and writes an updated combined table for 
#' further manual completion.
#'
#' @details
#' The purpose of this function is to maintain a consistent workflow for 
#' manually curated tables (e.g., sensitivity tables such as `bird_table_sensitivity`). 
#' It performs three key tasks:
#' (1) identifies rows in `df_new` that contain missing values in the selected columns;  
#' (2) appends only new unique rows to the existing table (`df_old`), which may be 
#' provided as a file path or an in-memory data frame;  
#' (3) writes the updated combined table to a CSV file to allow further manual editing.
#'
#' The function ensures that manually curated metadata remain the “source of additional
#' info” while new data are progressively integrated.
#'
#' @title Update manual table with new missing-value rows
#'
#' @param cols Character vector of column names to inspect for missing values.
#' @param df_old A data frame or a file path pointing to a previous version of the table.
#' @param df_new A data frame containing the new set of records to check.
#'
#' @return A data frame containing the combined old + newly added rows.
#'
#' @author eryntw
#' @export


make_manual_table <- function(df_new, dir) {
  
  # --- 1. Create long table of missing values ---
  cols <- setdiff(names(df_new), c("search_term", "aub_Taxon_common_name_2"))
  
  tbl <- df_new %>%
    mutate(across(all_of(cols), as.character)) %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "trait",
      values_to = "value"
    ) %>%
    filter(is.na(value) | value %in% c("-999", "NAV")) %>%
    distinct()
  
  # --- 2. Search for processed manual table ---
  
  files <- list.files(path = dir, 
                      pattern = "\\.csv$",
                      full.names = TRUE)
  latest <- files[order(files, decreasing = TRUE)][1]
  
  # --- 3.1 When latest is NA → simple output ---
  if (is.na(latest)) {
    csv <- tbl
  } else {
    # --- 3.2 When latest exists, compute new rows ---
    processed <- readr::read_csv(latest, col_types = readr::cols())
    new_rows <- tbl %>%
      select(-value) %>%
      anti_join(processed %>% 
                  select(-value)) %>% 
      mutate(value = NA)
    
    # --- 4. Append ---
    csv <- bind_rows(processed, new_rows)
  }
  
  # --- 5. write with stamp ---
  write_with_stamp(csv, "mtable", dir, ext = "csv")
}
