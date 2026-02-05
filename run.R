library(dplyr)
library(targets)

envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> unlist() |> unname()
                        , update_env = TRUE
)

# make tars ------- 
# creates path for output folders/ writes _targets.yaml

tars_local_o <- envTargets::make_tars(settings = "settings/scale.yaml",
                                      store_base = fs::path("..")) 

tars_local <- purrr::map(
  tars_local_o,
  function(x) {
    x$store <- sub(
      "(envSens/)[^/]+/[^/]+/",
      "\\1",
      x$store
    )
    x
  }
)

## Write tars ------

tars <- c(tars_local)
envTargets::write_tars(tars)


# run everything ----------
# in _targets.yaml

purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
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