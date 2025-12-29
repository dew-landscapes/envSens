#' .. content for \description{
#' This function is aiming for joining animal databases, do not use it for plants.
#' This function joins trait database to the PIA/ALA names using two steps matching. The first step matches the exact scientific names used in PIA/ALA and excluded them from the second step. The second step fuzzy matches (by two character difference) the unmatched ones from the first step using a table documenting alternative names. The function outputs all taxa from the queried PIA/ALA names and includes a column indicating which step a species found a match (or not).
#' } (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param aub_trim
#' @param prefix
#' @return
#' @author eryntw
#' @export

# data A = primary dataset, column of search_term is required.
# data B = reference dataset, columns of Genus and Species are required.
# A_sci = columns in A for scientific name
# prefix = prefix added to the joining dataset as identifier
# alt = alternative name table, required to be loaded prior.

### Look up table

A <- sa_birds %>% select(taxa, vernacular_name) %>%   tidyr::separate(
  taxa,
  into = c("Genus", "Species"),
  sep = " ",
  remove = FALSE) %>% 
  rename(common = vernacular_name) %>% 
  arrange(Genus, Species, is.na(common)) %>% 
  distinct(Genus, Species, common, .keep_all = TRUE) %>% 
  mutate(common = dplyr::coalesce(common, "NoName"))

B <-  traitdata::australian_birds %>% 
  select(Genus, Species, X3_Taxon_common_name_2,
         X6_Subspecies_name_2, X96_Body_length_8) %>% 
  rename(common = X3_Taxon_common_name_2) %>% 
  filter(!is.na(Species)) %>% 
  arrange(Genus, Species, is.na(common)) %>% 
  distinct(Genus, Species, common, .keep_all = TRUE) %>% 
  mutate(common = dplyr::coalesce(common, "NoName"))

prefix = "t_"

# 0. Add prefix
B <- B %>% 
  rename_with(~ paste0(prefix, .x), 
              .cols = -c(Genus, Species, common)) %>% 
  rename_with(~ paste0("B_", .x), 
              .cols = c(Genus, Species, common))

# 1. Fuzzy match Genus + Species + common ROUND 1
match1 <- 
  fuzzyjoin::stringdist_inner_join(
    A,
    B,
    by = c("Genus" = "B_Genus", "Species" = "B_Species", "common" = "B_common"),
    max_dist = 3,
    method = "osa",
  ) %>% 
  dplyr::mutate(
    match_dist  = stringdist::stringdist(Genus, B_Genus, method = "osa") +
      stringdist::stringdist(Species, B_Species, method = "osa") +
      stringdist::stringdist(common, B_common, method = "osa") 
  )

# 1-1. First round unmatched
unmatch1 <- dplyr::anti_join(
  A,
  match1,
  by = c("Genus" = "B_Genus", "Species" = "B_Species")
) %>%
  select(Genus, Species, common) %>% 
  distinct()

# 2. Fuzzy mix Genus + Species + common
candidates <- unmatch1 %>%
  dplyr::cross_join(B) %>%
  dplyr::filter(
    Genus == B_Genus | Species == B_Species
  )

candidates_filtered <- candidates %>%
  dplyr::mutate(
    common_dist = stringdist::stringdist(common, B_common, method = "osa")
  ) %>%
  dplyr::filter(common_dist < 3) # antarctic tern vs arctic tern

unmatch2 <- anti_join(unmatch1, candidates_filtered, by = c("Genus", "Species")) %>% 
  mutate(search_term = paste(Genus, Species))

# 3. Find all the synonyms! (IF unmatch2 < 50 rows!) #####
syn = synonyms(unmatch2$search_term,
               db = "itis")
syn_df = synonyms_df(syn) %>% 
  select(`.id`, acc_name, syn_name) %>% 
  tidyr::pivot_longer(
    cols = c(acc_name, syn_name),
    names_to = "name_type",
    values_to = "name"
  ) %>% 
  distinct() %>% 
  dplyr::mutate(
    name_bi = stringr::str_extract(name, "^\\S+\\s+\\S+")
  ) %>% na.omit()

# 4. Last match by synonyms
unmatch2_join <-
  inner_join(syn_df,
             B %>% mutate(taxa = paste(B_Genus, B_Species)), 
             by = c("name_bi" = "taxa"))

unmatch3 <- unmatch2 %>% 
  filter(!search_term %in% unmatch2_join$.id)


