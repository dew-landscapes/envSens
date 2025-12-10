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

alt.names <- data.frame(
  search_term = c(
    "Antigone rubicunda",
    "Cinclosoma castanotum",
    "Coturnix ypsilophora",
    "Amytornis whitei",
    "Hylacola cauta",
    "Parvipsitta porphyrocephala",
    "Petroica boodang",
    "Ardea intermedia",
    "Lophochroa leadbeateri",
    "Spatula rhynchotis",
    "Hypotaenidia philippensis",
    "Amytornis modestus",
    "Zapornia tabuensis",
    "Oedura cincta",
    "Morelia imbricata",
    "Tympanocryptis tolleyi",
    "Nyctophilus corbeni"
  ),
  
  alt_name = c(
    "Grus rubicunda",
    "Cinclosoma castanotus",
    "Synoicus ypsilophorus",
    "Amytornis striatus",
    "Calamanthus cauta",
    "Glossopsitta porphyrocephala",
    "Petroica multicolor",
    "Mesophoyx intermedia",
    "Cacatua leadbeateri",
    "Anas rhynchotis",
    "Gallirallus philippensis",
    "Amytornis textilis",
    "Porzana tabuensis",
    "Oedura marmorata",
    "Morelia spilota",
    "Tympanocryptis lineata",
    "Nyctophilus timoriensis")
)

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
