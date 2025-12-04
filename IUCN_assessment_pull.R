### This script generate IUCN assessments for a set class within Australia ####

setclass = "Aves"

###########################################################################

library(rredlist)
library(dplyr)
library(tidyr)

api_key <- Sys.getenv("IUCN_REDLIST_KEY")

if (is.null(api_key) || api_key == "") {
  stop(
    "IUCN API key not found.\n",
    "Please set your API key using:\n",
    'Sys.setenv(IUCN_REDLIST_KEY = "YOUR_KEY_HERE")\n',
    "You can obtain a free key from: https://api.iucnredlist.org/"
  )}

au_assessments <- rl_countries(code = "AU", quiet = TRUE, latest = TRUE)

class_assessments <- rl_class(class = setclass, latest = TRUE, quiet = TRUE)

assessment_ids <- intersect(au_assessments$assessments$assessment_id,
                            class_assessments$assessments$assessment_id)

assessment_list <- lapply(assessment_ids,
                          function(x) {
                            Sys.sleep(0.5)
                            rl_assessment(x)
                          }
)

assessment_df <- rl_assessment_extract(assessment_list, 
                                       c("taxon", 
                                         "red_list_category__code",
                                         "population_trend",
                                         "habitats",
                                         "threats",
                                         "conservation_actions",
                                         "stresses",
                                         "growth_forms",
                                         "systems"
                                         ), 
                                       format = "df", flatten = FALSE)

id <- assessment_df %>% 
  select(assessment_id, code, taxon) %>% 
  unnest_wider(taxon)

threat <- assessment_df %>% 
  select(assessment_id, threats) %>% 
  unnest(threats)

