rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/methodology_utils.R")

test_that(
  "Verifying data_source_tbl_html works without issue",
  expect_silent(data_source_tbl_html())
)
