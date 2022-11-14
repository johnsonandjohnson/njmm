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

## Set variables that would normally come from global
# Set the variables that will be checked for mmr_df_explore_more function and create_custom_pal function (test_mmr_utils.R & test_color_utils.R)
MMRATE_PER_1KYL <- "mmrate_per_1kyl"
MMRATIO_PER_100KLB <- "mmratio_per_100klb"

# Set colors that create_custom_pal uses to test (test_colors_utils.R)
COLORS_MMRATIO <- "Reds" 


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
