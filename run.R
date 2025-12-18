
library(targets)

envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> unlist() |> unname()
                        , update_env = TRUE
)

# make tars -------
## envSens

tars_local <- envTargets::make_tars(settings = "settings/scale.yaml") # creates path for output folders/ writes _targets.yaml

## Other project imports

tars_clean <- envTargets::make_tars(settings = yaml::read_yaml("settings/scale.yaml")
                                    , project_base = fs::path("..", "envCleaned")
                                    , local = TRUE #to get around naming
                                    , save_yaml = FALSE)

# tars_pia <- envTargets::make_tars(settings = yaml::read_yaml("settings/scale.yaml")
#                                     , project_base = fs::path("..", "envPIA")
#                                     , local = TRUE #to get around naming
#                                     , save_yaml = FALSE)

## collect and write tars

tars <- c(tars_local, tars_clean)
envTargets::write_tars(tars)


# run everything ----------
# in _targets.yaml

purrr::walk2(purrr::map(tars, "script")
             , purrr::map(tars, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
)


if(FALSE) {
  
  # individual tar_make-------
  
  script <- "import"
  
  tar_visnetwork(script = tars[[script]]$script
                 , store = tars[[script]]$store
                 , label = "time"
  )
  
  tar_make(script = tars[[script]]$script
           , store = tars[[script]]$store
  )
  
  tar_prune(script = tars[[script]]$script
            , store = tars[[script]]$store
  )
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store)
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars[[script]]$store)
  
}