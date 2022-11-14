## This script combines all layer data into one usable dataframe

AHRF_dates <- paste(first(AHRF_YEARS), last(AHRF_YEARS), sep = "-")
AHRF_dates_underscore <- paste(first(AHRF_YEARS), last(AHRF_YEARS), sep = "_")

# File path for ARHF data used for health insurance, medicaid insurance, and poverty layers
# (adapted from https://github.com/jjchern/ahrf/blob/master/data-raw/prep_county.R)
doc_file <- paste0("data/raw/ahrf_", AHRF_dates_underscore, "/DOC/AHRF ", AHRF_dates, " Technical Documentation.xlsx")
data_file <- paste0("data/AHRF_", AHRF_dates, "/DATA/AHRF", last(AHRF_YEARS), ".asc")

source("./etl/layers/layer_insurance.R")
source("./etl/layers/layer_poverty.R")
source("./etl/layers/layer_obgyns.R")
source("./etl/layers/layer_midwives.R")
source("./etl/layers/layer_csection.R")

# Primary/Foreign Keys to join on
key_columns <- c("year", "FIPS_Code")

# Create Other Layer Dataframe
other_layer_df <-
  insurance_cov_any %>%
  full_join(insurance_cov_medicaid, key_columns) %>%
  full_join(obgyns_per_capita, key_columns) %>%
  full_join(midwives_per_capita, key_columns) %>%
  full_join(poverty_percent, by = key_columns) %>%
  full_join(ntsv_csection_rates, key_columns) %>%
  rename(
    pct_medicaid = with_insurance_pct,
    pct_uninsured = no_insurance_pct,
    per_capita_obgyns = per_capita_100K.x,
    per_capita_midwives = per_capita_100K.y,
  ) %>%
  select(
    year, FIPS_Code,
    pct_uninsured,
    pct_medicaid,
    per_capita_obgyns,
    per_capita_midwives,
    ntsv_goal_diff,
    percent_poverty
  )

# save other layer df to RDS
saveRDS(other_layer_df, "data/processed/other_layers.rds")
