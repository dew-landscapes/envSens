
## Read all the species in SA
tarchetypes::tar_file_read(name = sa_species,
                           command = fs::path(tars$clean$store, "objects", "bio_clean"),
                           read = arrow::open_dataset(!!.x) %>% 
                             filter(grepl(" ", taxa)) %>% 
                             distinct() %>% 
                             collect())