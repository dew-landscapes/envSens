#' Multi-stage fuzzy taxonomic matching
#'
#' Performs a multi-round matching workflow between two taxonomic tables using:
#' (1) strict fuzzy matching on Genus + Species + common name;
#' (2) relaxed matching requiring exact Genus OR Species with fuzzy common name;
#' (3) synonym-based matching using ITIS-derived synonym tables.
#'
#' @param A Data frame containing columns `Genus`, `Species`, and `common`
#' @param B Data frame containing columns `B_Genus`, `B_Species`, and `B_common`
#' @param max_dist Integer. Maximum string distance for fuzzy joins (default = 3)
#' @param synonym_limit Integer. Maximum number of taxa allowed for synonym matching
#'
#' @return A list with elements:
#' \itemize{
#'   \item `match1` – first-round fuzzy matches
#'   \item `unmatch1` – taxa unmatched after round 1
#'   \item `match2` – relaxed fuzzy matches
#'   \item `unmatch2` – taxa unmatched after round 2
#'   \item `syn_matches` – matches found via synonyms
#'   \item `unmatch3` – final unmatched taxa
#' }
#'
#' @author eryntw
#' @export
multi_fuzzy_match <- function(A,
                              B,
                              max_dist = 3) {
  
  ## ---- 1. Round 1: strict fuzzy (Genus + Species + common) ----
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
        stringdist::stringdist(common,  B_common,  method = "osa"),
      match = "r1"
    )
  
  ## ---- 1-1. Unmatched after round 1 ----
  unmatch1 <- dplyr::anti_join(
    A,
    match1,
    by = c("Genus" = "B_Genus", "Species" = "B_Species")
  ) %>%
    dplyr::select(Genus, Species, common) %>%
    dplyr::mutate(match = "unmatch_r1") %>% 
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
    dplyr::mutate(match = "r2")
  
  ## ---- 2-1. Unmatched after round 2 ----
  unmatch2 <- dplyr::anti_join(
    unmatch1,
    match2,
    by = c("Genus", "Species")
  ) %>%
    dplyr::mutate(taxa = paste(Genus, Species),
                  match = "unmatch_r2")
  
  ## ---- 3. Synonym matching (guarded) ----
  
  synonyms <- readr::read_csv("data/synonyms.csv", col_types = readr::cols())
  
  syn_matches <- unmatch2 %>%
    dplyr::inner_join(synonyms, by = c("taxa" = ".id")) %>%
    tidyr::separate(
      name_bi,
      into = c("Genus", "Species"),
      sep = " ",
      remove = FALSE
    ) %>%
    fuzzyjoin::stringdist_left_join(
      B,
      by = c("Genus" = "B_Genus", "Species" = "B_Species"),
      max_dist = 2,
      method = "osa"
    ) %>%
    dplyr::filter(!is.na(B_Genus)) %>%
    dplyr::mutate(
      non_na = rowSums(!is.na(dplyr::across(everything()))),
      match = "r3"
    ) %>%
    dplyr::group_by(taxa) %>%
    dplyr::slice_max(non_na, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(-non_na, -name, -name_bi)
  
  ## ---- 3-1. Unmatched after round 3 ----
  unmatch3 <- dplyr::anti_join(
    unmatch2,
    syn_matches,
    by = "taxa"
  ) %>% 
    dplyr::mutate(match = "unmatch_r3") %>% 
    
    ## ---- Combine ----
  
  matched <- bind_rows(match1, match2, syn_matches)
  unmatched <- bind_rows(unmatch1, unmatch2, unmatch3)
  
  ## ---- return ----
  return(list(
    matched = matched
    unmatched = unmatched
  ))
}
