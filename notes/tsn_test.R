install.packages("ritis") # uncomment if not already installed
install_github('taxize_', 'ropensci') # uncomment if not already installed
install.packages("taxize", type="source") # uncomment if not already installed
library(dplyr)
library(ritis)
library(taxize)


pia <- sp_tables[["Aves"]] %>% 
  select(search_term, pia_epbc_status, ala_vernacular_name) %>% 
  distinct()
birdbase_trim <- birdbase %>% 
  select(Genus, Species, Disp, 
         `English Name (BirdLife > IOC > Clements>AviList)`) %>% 
  mutate(taxa = paste(Genus, Species))
  
  
  matched1 <- inner_join(pia, 
                         birdbase_trim, 
                         by = c("search_term" = "taxa"))

  unmatch1 <- dplyr::anti_join(
    pia,
    matched1,
    by = "search_term")

tsn = get_tsn_(unmatch1$ala_vernacular_name, 
               searchtype = "common", 
               ask = FALSE) %>% bind_rows()
tsn2 = get_tsn_(unmatch1$search_term, 
                searchtype = "scientific", 
                ask = FALSE) %>% bindrows()


tsn.df = dplyr::bind_rows(tsn)
tsn.df2 = dplyr::bind_rows(tsn2)
resolves = gna_verifier(names = unmatch1$search_term)
list = lapply(aa, gna_verifier)
df = bind_rows(list)
common = get_tsn_(bb_common, searchtype = "common", ask = FALSE) %>% bind_rows()

# search all names for the unmatched
unmatch1_names <- gna_verifier(unmatch1$search_term, all_matches = TRUE) %>% 
  filter(!currentCanonicalFull=="") %>% 
  select(submittedName, currentCanonicalFull, taxonomicStatus, isSynonym, matchType)

unmatch1_common = sci2comm(unmatch1_names$currentCanonicalFull) # not good

# Find all the synonyms!
syn = synonyms(unmatch1$search_term,
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

## common names matching table
unmatch_long <- unmatch1 %>%
  dplyr::transmute(
    .id = search_term,
    name = stringi::stri_enc_toutf8(ala_vernacular_name),
    name_type = "common"
  ) %>%
  dplyr::filter(!is.na(name), name != "")

## check the second join

unmatch_join <-
  inner_join(syn_df,
             birdbase_trim, 
             by = c("name_bi" = "taxa"))

unmatch2 <- unmatch1 %>% 
  filter(!search_term %in% unmatch_join$.id)


