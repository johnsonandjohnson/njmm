# Libraries
pacman::p_load(dplyr, readr, readxl, tidyr)

# NJ NTSV Goal - derived from 2020 Healthy People - https://www.healthypeople.gov/2020/topics-objectives/objective/mich-71
NJ_NTSV_Goal <- 24.7

# NJ County Reference
nj_ref <- read_csv("data/reference/nj_county_reference.csv", col_types = cols())

# Read in csection data
ntsv_csection_rates <- read_excel("data/raw/nj_ntsv_rates/NTSV Cesarean Section Rates 2000-2017.xlsx", skip = 9, col_types = "text")

# Grab last column with the totals, and make variable of years to exclude
total_column <- ntsv_csection_rates[, ncol(ntsv_csection_rates)]
years_to_exclude <- as.character(c(2000:2009))

# Clean up data
ntsv_csection_rates <- ntsv_csection_rates %>%
  rename(County_Name = ...1) %>%
  filter(!is.na(total_column) & County_Name != "Total") %>%
  select(-names(total_column), -all_of(years_to_exclude)) %>%
  left_join(nj_ref %>% select(County_Name, FIPS_Code), ., by = "County_Name") %>%
  pivot_longer(., cols = starts_with("2"), names_to = "year", values_to = "rate_csection")

# Calculate rate to percentage
ntsv_csection_rates <- ntsv_csection_rates %>%
  mutate(
    rate_csection = as.double(rate_csection),
    pct_csection = (rate_csection * 100),
    ntsv_goal_diff = round(pct_csection - NJ_NTSV_Goal, 3),
    year = as.numeric(year),
    FIPS_Code = as.character(FIPS_Code)
  )
