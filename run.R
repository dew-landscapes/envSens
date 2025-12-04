
library(targets)

packages <- yaml::read_yaml(here::here("settings","packages.yaml"))$packages

envFunc::check_packages(packages
                        , update_env = TRUE
                        , lib = TRUE)

# make tars -------
## envPIA
tars_local <- envTargets::make_tars("settings/context.yaml"
                                    , list_names = c("extent", "grain", "aoi")
                                    , project_base = fs::path("..", basename(here::here()))
                                    , save_yaml = FALSE)

## Other project imports
tars_occ <- envTargets::make_tars(settings = list(extent = yaml::read_yaml("settings/context.yaml")$extent[1:5])
                                  , project_base = fs::path("..", "envOcc")
                                  , store_base = fs::path("H:", "data")
                                  , local = FALSE
                                  , list_names = "extent")

tars_clean <- envTargets::make_tars(settings = yaml::read_yaml("settings/context.yaml")
                                    , project_base = fs::path("..", "envCleaned")
                                    , local = TRUE #to get around naming
                                    , save_yaml = FALSE)

tars_range <- envTargets::make_tars(settings = yaml::read_yaml("settings/context.yaml")
                                    , project_base = fs::path("..", "envRange")
                                    , local = TRUE
                                    , save_yaml = FALSE)

tars_regcont <- envTargets::make_tars(settings = list(extent = yaml::read_yaml("settings/context.yaml")$extent,
                                                      grain = yaml::read_yaml("settings/context.yaml")$grain,
                                                      aoi = yaml::read_yaml("settings/context.yaml")$aoi[1:4])
                                      , project_base = fs::path("..", "envRegCont")
                                      , local = TRUE
                                      , save_yaml = FALSE
                                      , list_names = c("extent", "grain", "aoi"))

tars_sdm <- envTargets::make_tars(settings = list(extent = yaml::read_yaml("settings/context.yaml")$extent,
                                                  grain = yaml::read_yaml("settings/context.yaml")$grain,
                                                  aoi = yaml::read_yaml("settings/settings.yaml")$env$aoi)
                                  , project_base = fs::path("..", "envSDMs")
                                  , local = FALSE
                                  , list_names = c("extent", "grain", "aoi"))

## collect and write tars
tars <- c(tars_local,
          tars_occ,
          list(envCleaned = list(species = tars_clean,
                                 subspecies = purrr::modify_tree(tars_clean, leaf = \(x) gsub("species", "subspecies", x))
          )
          ),
          list(envRange = list(species = tars_range,
                               subspecies = purrr::modify_tree(tars_range, leaf = \(x) gsub("species", "subspecies", x))
          )
          ),
          list(envRegCont = list(species = tars_regcont,
                               subspecies = purrr::modify_tree(tars_regcont, leaf = \(x) gsub("species", "subspecies", x))
          )
          ),
          tars_sdm
)


envTargets::write_tars(tars)



# run everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars, "script")
             , purrr::map(tars, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
             )

if(FALSE) {
  
  tars <- yaml::read_yaml("_targets.yaml")

  # individual tar_make-------
  script <- "concern"
  
  tar_visnetwork(script = tars[[script]]$script
                 , store = tars[[script]]$store
                 , targets_only = TRUE
  )
  
  tar_make(script = tars[[script]]$script
           , store = tars[[script]]$store
  )
  
  # tar_prune(script = tars[[script]]$script
  #           , store = tars[[script]]$store
  # )

  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store)
  

  # what is running?
  # run these from another instance of R while tar_make is running
  tar_dispatched(store = tars[[script]]$store)
  tar_progress_summary(store = tars[[script]]$store)
  
}