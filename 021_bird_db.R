library(targets)
library(tarchetypes)
library(crew)
library(crew.cluster)

use_cores <- parallel::detectCores() - 2

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = use_cores),
  workspace_on_error = TRUE # inspect the error using tar_traceback(target)
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

splist <- tar_read(splist, store = tars$taxa$store)
sa_birds <- tar_read(sa_birds, store = tars$taxa$store)

tar_plan(
  
  ## Static database -------
  
  ## Bird Base 2025 ------
  
  tarchetypes::tar_file_read(name = birdbase,
                             command = "database/data_BIRDBASE v2025.1 Sekercioglu et al. Final.csv",
                             read = readr::read_csv(file = !!.x, 
                                                    col_types = readr::cols(),
                                                    locale = readr::locale(encoding = "ASCII")) %>% 
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(commoncol = EnglishNameBirdLifeIocClementsAviList) %>% 
                               get_birdbase() # ATTENTION
  ),
  
  ## Birdlife Generation Length 2025 ------
  
  tarchetypes::tar_file_read(name = genlength,
                             command = "database/latest_generation_lengths_of_the_world's_birds_2025.xlsx",
                             read = readxl::read_excel(path = !!.x, 
                                                       sheet = 1,
                                                       skip = 1,
                                                       col_types = "guess") %>%
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(commoncol = EnglishName2024,
                                             taxa = SpeciesName2024)
  ),
  
  ## Birdlife Extent of Occurrence 2020 ------
  
  tarchetypes::tar_file_read(name = eoo,
                             command = "database/EOO_cobi13486-sup-0003-tables3.csv",
                             read = readr::read_csv(file = !!.x, 
                                                    col_types = readr::cols()) %>% 
                               janitor::clean_names(case = "upper_camel") %>% 
                               clean_taxa_df(taxa = ScientificName)
  ),  # eoo is messy; required better data from BirdLife
  
  ## Australian Birds 2015
  
  tar_target(name = ausbird,
             command = traitdata::australian_birds %>%
               setNames(gsub("^X\\d+_", "", names(.))) %>%
               dplyr::filter(Extinct_4 == 0) %>% 
               janitor::clean_names(case = "upper_camel") %>% 
               clean_taxa_df(commoncol = TaxonCommonName2) %>% 
               filter(is.na(SubspeciesName2)) # Most subspecies have no data
  ),
  
  ## Match database species -------
  
  tar_file_read(name = syn_db, 
                command = fs::path("data/synonyms.csv"),
                read = readr::read_csv(!!.x, col_types = readr::cols())
  ),
  
  ## join database: sa_birds -------
  tar_target(name = joined_table,
             command = sa_birds %>%
               join_database_(birdbase, prefix = "bb_", syn_db = syn_db) %>%
               join_database_(genlength, prefix = "bl_", syn_db = syn_db) %>%
               join_database_(eoo, prefix = "bl_", syn_db = syn_db) %>%
               join_database_(ausbird, prefix = "aub_", syn_db = syn_db) %>%
               dplyr::mutate(
                 bl_eoo_log10Scaled = scales::rescale(log10(bl_ExtentOfOccurrenceBreedingResident),
                                                      to = c(0, 1), na.rm = TRUE),
                 bl_genlen_logScaled = scales::rescale(log(bl_GenerationLength),
                                                       to = c(0, 1), na.rm = TRUE)
               )

  ),
  
  ## join database: pilot areas -------
  pilot_subset = left_join(splist, joined_table, by = "search_term")
  
)



