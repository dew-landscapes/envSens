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

targets_to_read <- names %>%
  filter(!str_ends(name, "_file")) %>%
  pull(name)

df_list <- lapply(names$name, function(x) tar_read(x, store = "setup"))


tar_plan(

  tar_target(name = df,
             command = pia_bp %>% bind_rows(pia_usg))
)



