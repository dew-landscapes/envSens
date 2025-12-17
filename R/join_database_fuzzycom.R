#' Multi-stage fuzzy taxonomic matching
#'
#' Performs a multi-round matching workflow between two taxonomic tables using:
#' (1) strict fuzzy matching on Genus + Species + common name;
#' (2) relaxed matching requiring exact Genus OR Species with fuzzy common name;
#' (3) synonym-based matching using ITIS.
#'
#' This function is designed for resolving unmatched taxa after standard joins,
#' particularly when names differ slightly or synonyms are involved.
#'
#' @param A Data frame containing columns `Genus`, `Species`, and `common`
#' @param B Data frame containing columns `B_Genus`, `B_Species`, and `B_common`
#' @param max_dist Integer. Maximum string distance for fuzzy joins (default = 3)
#' @param synonym_db Character. Database passed to `taxize::synonyms()` (default = "itis")
#' @param synonym_limit Integer. Maximum number of rows allowed for synonym search
#'
#' @return A list with elements:
#' \itemize{
#'   \item `match1` – first-round fuzzy matches
#'   \item `unmatch1` – taxa unmatched after round 1
#'   \item `candidates` – relaxed fuzzy candidates
#'   \item `unmatch2` – taxa still unmatched
#'   \item `syn_matches` – matches found via synonyms
#'   \item `unmatch3` – final unmatched taxa
#' }
#'
#' @author eryntw
#' @export
#'
multi_fuzzy_match <- function(A,
                              B,
                              max_dist = 3,
                              synonym_limit = 50) {
  
  ## ---- 1. Round 1: strict fuzzy match (Genus + Species + common) ----
  match1 <- fuzzyjoin::stringdist_inner_join(
    A,
    B,
    by = c(
      "Genus"   = "B_Genus",
      "Species" = "B_Species",
      "common"  = "B_common"
    ),
    max_dist = max_dist,
    method = "osa"
  ) %>%
    dplyr::mutate(
      match_dist =
        stringdist::stringdist(Genus,   B_Genus,   method = "osa") +
        stringdist::stringdist(Species, B_Species, method = "osa") +
        stringdist::stringdist(common,  B_common,  method = "osa")
    ) %>% 
    select(-match_dist) %>% 
    mutate(match = "r1")
  
  ## ---- 1-1. Unmatched after round 1 ----
  unmatch1 <- dplyr::anti_join(
    A,
    match1,
    by = c("Genus" = "B_Genus", "Species" = "B_Species")
  ) %>%
    dplyr::select(Genus, Species, common) %>%
    dplyr::distinct()
  
  ## ---- 2. Relaxed match: exact Genus OR Species + fuzzy common ----
  match2 <- dplyr::cross_join(unmatch1, B) %>%
    dplyr::filter(
      Genus == B_Genus | Species == B_Species
    ) %>%
    dplyr::mutate(
      common_dist = stringdist::stringdist(common, B_common, method = "osa")
    ) %>%
    dplyr::filter(common_dist < max_dist) %>% 
    select(-common_dist) %>% 
    mutate(match = "r2")
  
  unmatch2 <- dplyr::anti_join(
    unmatch1,
    candidates,
    by = c("Genus", "Species")
  ) %>%
    dplyr::mutate(search_term = paste(Genus, Species))
  
  ## ---- 2. Synonym matching ----
  
  synonyms <- readr::read_csv("data/synonyms.csv")
  
  syn_match <- unmatch2 %>%
    mutate(taxa = paste(Genus, Species)) %>% 
    inner_join(synonyms, by = c("taxa" = ".id")) %>% 
    tidyr::separate(
      name_bi,
      into = c("Genus", "Species"),
      sep = " ",
      remove = FALSE) %>% 
    fuzzyjoin::stringdist_left_join(B, by = c("Genus" = "B_Genus", 
                                              "Species" = "B_Species"), 
                                    max_dist = 2, 
                                    method = "osa") %>% 
    filter(!is.na(B_Genus)) %>% 
    # keep rows with more data
    mutate(non_na = rowSums(!is.na(across(everything())))) %>%  
    group_by(taxa) %>%
    slice_max(non_na, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(match = "r3") %>% 
    select(-non_na, name, name_bi)
  
  ## ---- return ----
  return(list(
    match1     = match1,
    unmatch1   = unmatch1,
    match2 = candidates_filtered,
    unmatch2   = unmatch2,
    syn_matches = syn_matches,
    unmatch3   = unmatch3
  ))
}
