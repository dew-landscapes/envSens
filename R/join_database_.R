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

join_database_ <- function(A,
                            B,
                            prefix,
                            max_dist = 3) {
  
  ## ---- 0. Ensure UTF-8 (defensive, column-wise) ----
  A <- A %>% mutate(across(where(is.character), ~ stringi::stri_enc_toutf8(.x)))
  B <- B %>% mutate(across(where(is.character), ~ stringi::stri_enc_toutf8(.x)))
  
  ## ---- 0.1 Detect common availability ----
  has_common_A <- "common" %in% names(A)
  has_common_B <- "common" %in% names(B)
  use_common   <- has_common_A && has_common_B
  
  ## ---- 0.2 Add prefix to B ----
  B <- B %>%
    rename_with(~ paste0(prefix, .x),
                .cols = -intersect(c("Genus", "Species", "common"), names(B))) %>%
    rename_with(~ paste0("B_", .x),
                .cols = intersect(c("Genus", "Species", "common"), names(B)))
  
  ## ---- 1. Round 1: strict fuzzy ----
  if (use_common) {
    
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
      mutate(
        match_dist =
          stringdist::stringdist(Genus,   B_Genus,   method = "osa") +
          stringdist::stringdist(Species, B_Species, method = "osa") +
          stringdist::stringdist(common,  B_common,  method = "osa"),
        match = "r1"
      )
    
  } else {
    
    match1 <- fuzzyjoin::stringdist_inner_join(
      A,
      B,
      by = c(
        "Genus"   = "B_Genus",
        "Species" = "B_Species"
      ),
      max_dist = max_dist,
      method = "osa"
    ) %>%
      mutate(
        match_dist =
          stringdist::stringdist(Genus,   B_Genus,   method = "osa") +
          stringdist::stringdist(Species, B_Species, method = "osa"),
        match = "r1"
      )
  }
  
  ## ---- 1-1. Unmatched after round 1 ----
  unmatch1 <- anti_join(
    A,
    match1,
    by = c("Genus" = "B_Genus", "Species" = "B_Species")
  ) %>%
    distinct(Genus, Species, .keep_all = TRUE) %>%
    mutate(match = "un_r1")
  
  ## ---- 2. Relaxed match ----
  if (use_common) {
    
    match2 <- cross_join(unmatch1, B) %>%
      filter(Genus == B_Genus | Species == B_Species) %>%
      mutate(common_dist = stringdist::stringdist(common, B_common, method = "osa")) %>%
      filter(common_dist < max_dist) %>%
      mutate(match = "r2")
    
  } else {
    
    match2 <- cross_join(unmatch1, B) %>%
      filter(Genus == B_Genus | Species == B_Species) %>%
      mutate(match = "r2")
  }
  
  ## ---- 2-1. Unmatched after round 2 ----
  unmatch2 <- anti_join(
    unmatch1,
    match2,
    by = c("Genus", "Species")
  ) %>%
    mutate(
      taxa = paste(Genus, Species),
      match = "un_r2"
    )
  
  ## ---- 3. Synonym matching ----
  synonyms <- readr::read_csv("data/synonyms.csv", col_types = readr::cols())
  
  syn_matches <- unmatch2 %>%
    inner_join(synonyms, by = c("taxa" = ".id")) %>%
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
    filter(!is.na(B_Genus)) %>%
    mutate(
      non_na = rowSums(!is.na(across(everything()))),
      match = "r3"
    ) %>%
    group_by(taxa) %>%
    slice_max(non_na, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-non_na, -name, -name_bi)
  
  ## ---- 3-1. Unmatched after round 3 ----
  unmatch3 <- anti_join(
    unmatch2,
    syn_matches,
    by = "taxa"
  ) %>%
    mutate(match = "un_r3")
  
  ## ---- Combine ----
  list(
    match   = bind_rows(match1, match2, syn_matches),
    unmatch = bind_rows(unmatch1, unmatch2, unmatch3)
  )
}
