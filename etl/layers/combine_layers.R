## This script combines all layer data into one usable dataframe

# File path for ARHF data used for health insurance, medicaid insurance, and poverty layers
# (adapted from https://github.com/jjchern/ahrf/blob/master/data-raw/prep_county.R)
doc_file <- "data/raw/ahrf_2018_2019/DOC/AHRF 2018-2019 Technical Documentation.xlsx"
data_file <- "data/AHRF_2018-2019/DATA/AHRF2019.asc"

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
