# Libraries
pacman::p_load(dplyr, janitor, purrr, readr, readxl, rlang, tidyr)

# Sources
source("./etl/woman_years_lived.R")

# NJ County Reference
nj_ref <- read_csv("data/reference/nj_county_reference.csv", col_types = cols()) %>%
  select(County_Name, FIPS_Code)

## Maternal Deaths
# Grab maternal deaths to be used as the numerator for both MMRatio and MMRate
maternal_deaths <- read_csv("data/raw/nj_maternal_deaths/2005-2017 Deaths by County.csv",
  skip = 10,
  col_types = cols()
)

# Clean up messy SHAD data
maternal_deaths <- maternal_deaths %>%
  rename(County_Name = X1) %>%
  filter(!is.na(X15) & County_Name != "Total") %>%
  select(-X15) %>%
  left_join(nj_ref, ., by = "County_Name") %>%
  type_convert(col_types = cols()) %>%
  pivot_longer(., cols = starts_with("2"), names_to = "year", values_to = "maternal_deaths")


## Live Births
# Grab Live Births data for denominator of MMRatio
live_births <- read_csv("data/raw/nj_live_births/2005-2017 Live Births by County.csv",
  skip = 9, col_types = cols()
)

# Clean up messy SHAD Live Births data
live_births <- live_births %>%
  rename(County_Name = X1) %>%
  filter(!is.na(X15) & County_Name != "Total") %>%
  select(-X15) %>%
  left_join(nj_ref, ., by = "County_Name") %>%
  type_convert(col_types = cols()) %>%
  pivot_longer(., cols = starts_with("2"), names_to = "year", values_to = "live_births")


## Calculate MMRatio
mmratio_df <- maternal_deaths %>%
  full_join(live_births, by = c("FIPS_Code", "County_Name", "year")) %>%
  complete(FIPS_Code, year) %>%
  mutate(
    maternal_deaths = replace(maternal_deaths, is.na(maternal_deaths), 0),
    mmratio = maternal_deaths / live_births,
    mmratio_per_100klb = floor(mmratio * 100000)
  )


## Woman Years Lived
# Grab woman years lived across all years and counties for denominator of MMRate
years_lived <- c(2005:2017) %>%
  map(woman_years_lived) %>%
  reduce(full_join, by = "fips_code")

# Create the column names for WYL specific to each year and replace
colnames(years_lived) <- c("fips_code", paste(2005:2017))

# Make years_lived df long instead of wide to get ready for the join to maternal_deaths df
years_lived <- years_lived %>%
  pivot_longer(., cols = starts_with("2"), names_to = "year", values_to = "years_lived")


## Calculate MMRate
mmrate_df <- maternal_deaths %>%
  full_join(years_lived, by = c("FIPS_Code" = "fips_code", "year")) %>%
  complete(FIPS_Code, year) %>%
  mutate(
    maternal_deaths = replace(maternal_deaths, is.na(maternal_deaths), 0),
    mmrate = maternal_deaths / years_lived,
    mmrate_per_1kyl = round(mmrate * 1000, 3)
  )


## Join MMRatio and MMRate into one df
mmr_df <- mmrate_df %>%
  full_join(mmratio_df, by = c("FIPS_Code", "County_Name", "year", "maternal_deaths"))


# save MMRate and MMRatio to RDS file
saveRDS(mmr_df, "data/processed/mmr.rds")
