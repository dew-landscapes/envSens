
library(targets)

# tars --------
## local ------
store_base <- envFunc::get_env_dir() |>
  fs::path_rel() |>
  fs::path(if(grepl("\\/prod\\/", here::here())) "prod" else "dev"
           , "out"
           )

tars <- envTargets::make_tars(settings = envFunc::extract_scale("envPIA")
                              , project_base = fs::path("..", "envPIA")
                              , store_base = store_base
                              , local = FALSE
                              )

stores <- fs::dir_ls(dirname(tars$envPIA$setup$store) # point at any store here as we're really just after the directory above each store
                     , regexp = "final\\/objects\\/final$"
                     , recurse = TRUE
                     )

tifs <- envTargets::collect_values(dir = unique(dirname(dirname(stores)))
                                   , object = "final\\/objects\\/final$"
                                   , collect_col = "use_tif"
                                   )
