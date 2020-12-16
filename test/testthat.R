if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  forcats,
  janitor,
  purrr,
  readr,
  readxl,
  rprojroot,
  testthat,
  tidyr
)

# Source global for title_utils functions
source("./global.R")

# Grab project root path
proj_root <- has_file("nj_mmr_pilot.Rproj") %>%
  find_root()

# Build path prefix to grab test data
path_prefix <- file.path(
  proj_root,
  "test",
  "test_data"
)

test_dir("./test/unit_tests/")
