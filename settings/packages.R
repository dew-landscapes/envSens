## library() calls go here
library(conflicted)
library(dotenv)
library(targets)
library(tarchetypes)

#### Ensure required packages are installed ####
pkgs_cran <- c("rredlist", 
               "purrr", 
               "stringr", 
               "tidyr", 
               "dplyr", 
               "readxl", 
               "galah", 
               "readr",
               "renv",
               "broom")

##################################
# install CRAN packages if missing
for (pkg in pkgs_cran) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
##################################