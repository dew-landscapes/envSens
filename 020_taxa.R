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
  filter(!grepl("_file$", name)) %>%
  pull(name)
list = c()
for (i in 1:length(targets_to_read)){
  list[i] = tar_read(as.symbol(targets_to_read[i]), store = "setup")
  
}


tar_plan(

  tar_target(name = df,
             command = pia_bp %>% bind_rows(pia_usg))
)



