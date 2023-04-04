rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/leaflet_utils.R")

test_that(
  "Testing Basic NJ Map",
  expect_silent(basic_map_NJ())
)

test_that(
  "Testing Highlights",
  expect_silent(map_highlight())
)

test_that(
  "Testing Labels",
  expect_silent(map_label())
)

# Load birth hospital file
BIRTH_HOSPS <<- readRDS("data/processed/hospitals_with_delivery.rds")
test_that(
  "Testing Adding Hospital Layer",
  expect_silent(leaflet() %>% hospital_layer())
)

# Read in other layer df
OTHER_LAYER_DF <- readRDS("data/processed/other_layers.rds")
pct_uninsured <- OTHER_LAYER_DF %>%
  filter(year == 2017) %>%
  pull(pct_uninsured)

COUNTIES <<- readRDS("data/processed/nj_counties_simplified.RDS")
test_that(
  "Testing Other Layers",
  {
    expect_message(
      leaflet() %>%
        add_polygons_layer_map("Blues", "Percent Uninsured", 100, pct_uninsured)
    )
  }
)
