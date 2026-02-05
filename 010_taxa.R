library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2
tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages
               , controller = crew_controller_local(workers = use_cores))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

targets <- list (
  
  ## Input species list options -------
  
  # All SA animals
  tarchetypes::tar_file_read(name = sa_animals,
                             command = fs::path("H:/dev/out/envCleaned/aus_imcra_prov_dissolve______0__P50Y__sa_br_dissolve/90__90__P1Y__species/clean/objects/bio_clean"),
                             read = arrow::open_dataset(!!.x) %>%
                               dplyr::filter(grepl(" ", taxa), kingdom == "Animalia") %>%
                               dplyr::select(taxa, common) %>%
                               dplyr::distinct() %>%
                               dplyr::collect()
  ),

  # All SA birds
  tar_target(name = sa_birds,
             command = galah::search_taxa(sa_animals$taxa) %>% 
               dplyr::distinct() %>% 
               dplyr::filter(class == "Aves") %>%
               replace_taxa(taxa_col = "search_term") %>% 
               clean_taxa_df(commoncol = vernacular_name,
                             taxacol = search_term) %>%
               dplyr::select(search_term, Genus, Species)
  ),

  # USG species
  tarchetypes::tar_file_read(name = usg,
                             command = fs::path("data/taxa_summary_Upper Spencer Gulf - Gawler Ranges.csv"),
                             read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  # BP species
  tarchetypes::tar_file_read(name = bp,
                             command = fs::path("data/taxa_summary_Braemer Province.csv"),
                             read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## Target species list ------
  tar_target(name = splist,
             command = usg %>% 
               dplyr::bind_rows(bp) %>% 
               organise_piaout() %>% 
               .$Aves %>% 
               replace_taxa(taxa_col = "search_term") %>% 
               clean_taxa_df(taxacol = search_term, 
                             commoncol = ala_vernacular_name)
  )
  
)
