#' Match taxonomic synonyms using ITIS
#'
#' This function queries ITIS for taxonomic synonyms using `taxize::synonyms()`.
#' It maintains a persistent CSV file of previously queried taxa and only
#' fetches new synonyms when new taxa are supplied.
#'
#' @param taxa Character vector of taxon names (e.g. "Genus species")
#' @param path File path to the synonyms CSV file
#'
#' @return A data.frame of synonyms with accepted and synonym names
#' @export
#'
#'
#'

## helper: fetch + tidy synonyms ------

fetch_synonyms <- function(x) {
  
  syn <- taxize::synonyms(x, db = "itis", ask = FALSE)
  
  # Step 1: standardize empty data frames
  lst_standard <- Map(
    function(df, nm) {
      if (is.data.frame(df) && nrow(df) == 0 && ncol(df) == 0) {
        data.frame(
          .id       = nm,
          sub_tsn    = NA_character_,
          acc_tsn    = NA_character_,
          syn_author = NA_character_,
          syn_name   = NA_character_,
          syn_tsn    = NA_character_,
          stringsAsFactors = FALSE
        )
      } else {
        df
      }
    },
    syn,
    names(syn)
  )
  
  # Step 2: bind rows with .id = "taxa"
  df_bound <- dplyr::bind_rows(lst_standard, .id = ".id") %>% 
    select(-matches("^dummy$")) %>% 
    dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, "NoData")))
  
  return(df_bound)
  
}

## main function ------

match_synonym <- function(taxa, path = "data/synonyms.csv") {
  
  # ensure unique input
  taxa <- unique(taxa)
  
  # ---- Case 1: no existing file ----
  if (!base::file.exists(path)) {
    
    message("No existing synonym file found — querying all taxa")
    
    syn_df <- fetch_synonyms(taxa)
    readr::write_csv(syn_df, path)
    
    return(syn_df)
  }
  
  # ---- Case 2: file exists ----
  processed <- readr::read_csv(path, col_types = readr::cols())
  
  new_taxa <- base::setdiff(taxa, processed$.id)
  
  if (length(new_taxa) == 0) {
    message("No new taxa to query — returning existing file")
    return(processed)
  }
  
  message("Querying synonyms for ", length(new_taxa), " new taxa")
  
  syn_new <- fetch_synonyms(new_taxa)
  
  combined <- dplyr::bind_rows(processed, syn_new) %>%
    dplyr::distinct()
  
  readr::write_csv(combined, path)
  
  return(combined)
}
