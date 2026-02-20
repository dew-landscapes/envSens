
library(targets)

# packages --------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> unlist() |> unname() |> unique()
                        , update_env = TRUE
                        )

# tars --------
## local ------
store_base <- envFunc::get_env_dir() |>
  fs::path_rel() |>
  fs::path(if(grepl("\\/prod\\/", here::here())) "prod" else "dev"
           , "out"
           )

tars_local <- envTargets::make_tars(settings = envFunc::extract_scale("envSens")
                                    , store_base = store_base
                                    , save_yaml = FALSE
                                    , list_names = "store"
                                    )

tars <- c(tars_local
          ## envCleaned --------
          , envTargets::make_tars(settings = envFunc::extract_scale(element = "envCleaned")
                                  , project_base = fs::path("..", "envCleaned")
                                  , store_base = store_base
                                  , local = FALSE
                                  )
          )

envTargets::write_tars(tars)


# run everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
             )

if(FALSE) {
  
  # individual tar_make-------
  
  script <- "setup"
  
  tar_visnetwork(script = tars_local[[script]]$script
                 , store = tars_local[[script]]$store
                 , label = "time"
                 , physics = TRUE
                 #, name = matches("Melithreptus_gularis")
                 )
  
  tar_make(script = tars_local[[script]]$script
           , store = tars_local[[script]]$store
           )
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars_local[[script]]$store) |>
    dplyr::inner_join(tibble::tibble(name = tar_errored(store = tars[[script]]$store)))
  
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars_local[[script]]$store)
  
  # Invalidate (i.e. to rerun) some targets
  if(FALSE) {
    
    # purrr::map(tars_local
    #            , \(x) tar_invalidate(matches("prep_|predict_|thresh_"), store = x$store)
    #            )
    
  }
  
}

if(FALSE) {
  
  # current run errors ------
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store) |>
    dplyr::inner_join(tibble::tibble(name = tar_errored(store = tars[[script]]$store)))
  
}


if(FALSE) {
  
  # check crew logs ------
  log_directory <- fs::path(tars[[script]]$store, "log")
  
  logs <- fs::dir_ls(log_directory
                     , recurse = TRUE
                     , regexp = "log$"
                     ) |>
    tibble::enframe(name = NULL, value = "path") |>
    dplyr::mutate(run = basename(dirname(dirname(path)))
                  , controller = basename(dirname(path))
                  , run_date = lubridate::ymd_hm(run)
                  ) |>
    dplyr::filter(run_date == max(run_date)) |>
    dplyr::mutate(log = purrr::map(path, \(x) readLines(x) |>
                                     paste0(collapse = "\n")
                                   )
                  , has_error = purrr::map_lgl(log, \(x) grepl("error", tolower(x)))
                  )
  
  error_lines <- logs |>
    dplyr::filter(has_error) |>
    dplyr::mutate(time = purrr::map(path, \(x) fs::file_info(x)$modification_time)) |>
    tidyr::unnest(cols = c(time)) |>
    dplyr::arrange(time)
  
  # View logs that had errors
  error_lines |>
    dplyr::pull(log) |>
    purrr::map(\(x) cat(x))
  
}

if(FALSE) {
  
  # prop done ------
  
  ## overall -------
  prop_done <- (tar_completed(store = tars$sdm$store) |>
    length() +
    (tar_skipped(store = tars$sdm$store) |>
       length()
     )) /
    (nrow(tar_manifest(script = tars$sdm$script)))
  
  ## predict -------
  (done <- tar_completed(names = matches("predict_fine"), store = tars$sdm$store) |>
    length() +
    (tar_skipped(names = matches("predict_fine"), store = tars$sdm$store) |>
       length()
     ))
  
  (todo <- nrow(tar_manifest(names = matches("predict_fine"), script = tars$sdm$script)))
  
  done / todo
  
}
