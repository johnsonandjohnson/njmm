rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/label_utils.R")

mmr_df <- read_rds("./test/test_data/label_utils_test/test_mmr_df.RDS")

# MMRATE
test_that(
  "Label outputs are as expected",
  expect_equal(
    build_map_label(mmr_df, "mmrate_per_1kyl"),
    read_rds("./test/test_data/label_utils_test/validated_label_mmrate.RDS")
  )
)

# MMRATIO
test_that(
  "Label outputs are as expected",
  expect_equal(
    build_map_label(mmr_df, "mmratio_per_100klb"),
    read_rds("./test/test_data/label_utils_test/validated_label_mmratio.RDS")
  )
)
