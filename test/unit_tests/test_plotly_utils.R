rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/plotly_utils.R")

test_mmr_df <- readRDS("test/test_data/plotly_utils_test/test_mmr_df.RDS")

test_that(
  "Verifying data_source_tbl_html works without issue",
  expect_silent(one_plot(test_mmr_df))
)
