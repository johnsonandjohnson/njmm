rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/color_utils.R")

test_mmr_values <- readRDS("data/processed/mmr.rds") %>%
  pull(mmratio_per_100klb)

pal_to_test <- create_custom_pal(test_mmr_values, "mmratio_per_100klb")
pal_stored <- readRDS("test/test_data/color_utils_test/mmratio_custom_palette.RDS")

test_that(
  "Testing Defaut MMRatio Color Palette",
  
  expect_equal(
    pal_to_test(test_mmr_values),
    pal_stored(test_mmr_values)
  )
)
