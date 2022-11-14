rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/color_utils.R")

test_mmr_values <- readRDS("test/test_data/color_utils_test/test_mmr_df_through2019.RDS") %>%
  pull(mmratio_per_100klb)

pal_to_test <- create_custom_pal(test_mmr_values, "mmratio_per_100klb")
pal_stored <- readRDS("test/test_data/color_utils_test/validated_mmratio_custom_palette.RDS")

test_that(
  "Testing Defaut MMRatio Color Palette",
  
  expect_equal(
    pal_to_test(test_mmr_values),
    pal_stored(test_mmr_values)
  )
)