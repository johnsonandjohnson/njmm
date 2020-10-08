rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/census_utils.R")

state_census_url_2005 <- "https://data.census.gov/cedsci/table?q=C03002%3A%20HISPANIC%20OR%20LATINO%20ORIGIN%20BY%20RACE&table=DP05&tid=ACSDT1Y2005.C03002&g=0400000US34&hidePreview=true&moe=true"

test_that(
  "Testing 2005 Statewide URL",
  expect_equal(
    build_census_tbl_url(2005, 34, "state"),
    state_census_url_2005
  )
)

bergen_census_url_2010 <- "https://data.census.gov/cedsci/table?q=C03002%3A%20HISPANIC%20OR%20LATINO%20ORIGIN%20BY%20RACE&table=DP05&tid=ACSDT1Y2010.C03002&g=0500000US34003&hidePreview=true&moe=true"
test_that(
  "Testing 2010 Bergen URL",
  expect_equal(
    build_census_tbl_url(2010, 34003, "county"),
    bergen_census_url_2010
  )
)
