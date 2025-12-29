
aubdata <- traitdata::australian_birds %>% 
  setNames(gsub("^X\\d+_", "", names(.))) %>%
  dplyr::filter(is.na(Subspecies_name_2)) %>% 
  dplyr::filter(Extinct_4 == 0) %>% 
  dplyr::mutate(original_name = paste(Genus, Species)) %>% 
  make_taxonomy(taxa_col = "original_name")

search_term <- aubdata %>% 
  .$species %>%
  .$lutaxa %>% 
  filter(returned_rank == "species")

unmatched <- setdiff(aubdata[["raw"]][["original_name"]], search_term$original_name)

sa_animals <- arrow::open_dataset(
  fs::path(tars$clean$store, "objects", "bio_clean")
) %>%
  dplyr::filter(grepl(" ", taxa), kingdom == "Animalia") %>%
  dplyr::select(taxa, common) %>%
  dplyr::distinct() %>%
  dplyr::collect() %>% 
  envClean::make_taxonomy(taxa_col = "taxa")

  {
    dplyr::filter(.$raw, class == "Aves") %>%
      dplyr::left_join(
        dplyr::select(.$species$lutaxa, original_name, taxa),
        by = "original_name"
      )
  }


# install.packages("ritis") # uncomment if not already installed
# install_github('taxize_', 'ropensci') # uncomment if not already installed
# install.packages("taxize", type="source") # uncomment if not already installed
library(ritis)
library(taxize)





# A must have Genus, Species, common

make_taxonomy_and_joindata <- function(A, B, 
                                       prefix) {
  
  # 0. Add prefix
  B <- data %>% 
    rename_with(~ paste0(prefix, .x), 
                .cols = -c(Genus, Species, common)) %>% 
    rename_with(~ paste0("B_", .x), 
                .cols = c(Genus, Species, common))
  
  # 1. First round matched: join by Genus and Species
  exactmatch <- 
    dplyr::inner_join(
      A,
      B,
      by = c("Genus" = "B_Genus", "Species" = "B_Species"),
      keep = TRUE
    ) %>% 
    dplyr::mutate(match = "exactMatch")
  
  # 1-1. First round unmatched
  unmatch1 <- dplyr::anti_join(
    A,
    exactmatch,
    by = c("Genus" = "B_Genus", "Species" = "B_Species")
  ) %>%
    select(Genus, Species, common) %>% 
    distinct()
  
  # 2. Second round: Join unmatched by fuzzy join
  fuzzymatch <- 
    fuzzyjoin::stringdist_inner_join(unmatch1,
                                     B, 
                                     by = c("Genus" = "B_Genus", "Species" = "B_Species"), 
                                     max_dist = 2, 
                                     method = "osa") %>% 
    dplyr::mutate(match = "fuzzyMatch")
  
  # 2-1 second round unmatched
  unmatch2 <- dplyr::anti_join(
    unmatch1,
    fuzzymatch,
    by = c("Genus", "Species")
  ) %>%
    select(Genus, Species, common) %>% 
    distinct()
  
  # 3. Third round: fuzzy match both common name and scientific name  
  fuzzymatch2 <- 
    fuzzyjoin::stringdist_inner_join(
      unmatch2 %>% dplyr::mutate(
      common = common %>% iconv(to = "UTF-8")
    ),
    B %>% dplyr::mutate(
      B_common = stringi::stri_enc_toutf8(B_common)
    ), 
    by = c("Genus" = "B_Genus", "common" = "B_common"), 
    max_dist = 2, 
    method = "osa") %>% 
    dplyr::mutate(match = "fuzzyMatch2")
  
  # 4. Organised the still unmatched in the second round
  second_unmatch <- first_unmatch %>% 
    filter(!search_term %in% second_match$search_term) %>% 
    left_join(B, by = setNames("B_sci", A_sci)) %>% 
    mutate(match = "unmatched")
  
  # 5. Bind first_match, second_match, and still unmatched df together
  result <- first_match %>% 
    bind_rows(second_match %>% select(-alt_name, -B_sci)) %>% 
    bind_rows(second_unmatch) %>% 
    select(-Genus, -Species)
  names(result)[names(result) == "match"] <- paste0(prefix, "match")
  
  return(result)
}





join_database <- function(A, B, A_sci = "search_term", alt, prefix) {
  
  # 0. Paste genus and species into a column, add prefix
  B <- B %>% mutate(B_sci = paste(Genus, Species)) %>% 
    rename_with(~ paste0(prefix, .x), 
                .cols = -c(Genus, Species, B_sci)) 
  
  # 1. First round matched: join by scientific name
  first_match <- A %>%
    left_join(B, by = setNames("B_sci", A_sci)) %>% 
    filter(!is.na(Genus)) %>% distinct() %>% 
    mutate(match = "first")
  
  # 2. First round unmatched
  first_unmatch <- A %>%
    filter(!search_term %in% first_match$search_term)
  
  # 3. Second round: Join unmatched by alternative names
  second_match <- first_unmatch %>% 
    left_join(alt, by = "search_term") %>% 
    filter(!is.na(alt_name)) %>% 
    fuzzyjoin::stringdist_left_join(B, by = c("alt_name" = "B_sci"), 
                                    max_dist = 2, 
                                    method = "osa") %>% 
    filter(!is.na(B_sci)) %>% 
    mutate(match = "second")
  
  # 4. Organised the still unmatched in the second round
  second_unmatch <- first_unmatch %>% 
    filter(!search_term %in% second_match$search_term) %>% 
    left_join(B, by = setNames("B_sci", A_sci)) %>% 
    mutate(match = "unmatched")
  
  # 5. Bind first_match, second_match, and still unmatched df together
  result <- first_match %>% 
    bind_rows(second_match %>% select(-alt_name, -B_sci)) %>% 
    bind_rows(second_unmatch) %>% 
    select(-Genus, -Species)
  names(result)[names(result) == "match"] <- paste0(prefix, "match")
  
  return(result)
}
