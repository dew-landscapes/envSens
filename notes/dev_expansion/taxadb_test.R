library(taxadb)
pia <- sa_birds %>% 
  mutate(
    cleaned_name = clean_names(search_term),
    lowercase = FALSE) %>%
  mutate(
    itis = taxadb::get_ids(search_term, "itis"),
    col = taxadb::get_ids(search_term, "col"),
    gbif = taxadb::get_ids(search_term, "gbif"))

multi_match <- filter_name(sa_birds$search_term, provider = "gbif")

duplicates <- multi_match %>% filter(duplicated(scientificName))

birdbase_1 <- birdbase %>% mutate(
  cleaned_name = clean_names(paste(Genus, Species),
                             lowercase = FALSE),
  itis = taxadb::get_ids(cleaned_name, "itis"),
  col = taxadb::get_ids(cleaned_name, "col"),
  gbif = taxadb::get_ids(cleaned_name, "gbif"))

library(rgbif)
name_backbone("Hylacola cauta")$usageKey
aa <- name_usage(key=2486547)$data # this will get all the synonyms

library(stringr)
pia <- pia %>%
  mutate(gbif_key = as.integer(str_remove(gbif, "^GBIF:")))

keys <- pia$gbif_key %>% na.omit()

dfs <- lapply(keys, function(k) {
  res <- name_usage(key = k)$data
  if (is.null(res) || nrow(res) == 0) return(NULL)
  res
})

combined_df <- do.call(rbind, dfs)

