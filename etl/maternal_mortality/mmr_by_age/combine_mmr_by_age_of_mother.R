## Libraries
pacman::p_load(dplyr, forcats, janitor, purrr, readr, readxl, tidyr)

## NJ County Reference
nj_ref <- read_csv("data/reference/nj_county_reference.csv", col_types = cols())

## Sources
source("./etl/maternal_mortality/mmr_by_age/mmratio_by_age_of_mother.R")
source("./etl//maternal_mortality/mmr_by_age/mmrate_by_age_of_mother.R")

## Join MMRatio and MMRate into one df
# Maternal deaths over 50 age group is not included in mmrate df as the denominator of wyl only includes ages 15-49
mmr_by_age_df <- mmrate_df %>%
  right_join(mmratio_df, by = c("county", "year", "age_of_mother", "maternal_deaths")) %>% 
  mutate(
    age_of_mother = as.character(age_of_mother)
  ) %>% 
  arrange(county, year, age_of_mother)

# Calculate All Counties of NJ summaries
overall_NJ <- mmr_by_age_df %>%
  ungroup() %>%
  group_by(year, age_of_mother) %>%
  summarise(
    maternal_deaths = sum(maternal_deaths, na.rm = TRUE),
    live_births = sum(live_births, na.rm = TRUE),
    years_lived = sum(years_lived, na.rm = TRUE)
  ) %>%
  mutate(county = "All Counties of NJ") %>%
  ungroup() %>%
  mutate(
    mmratio = maternal_deaths / live_births,
    mmratio_per_100klb = floor(mmratio * 100000),
    mmrate = ifelse(age_of_mother != "Over 50", maternal_deaths / years_lived, NA),
    mmrate_per_1kyl = ifelse(age_of_mother != "Over 50", round(mmrate * 1000, 3), NA)
  )

mmr_by_age_df <- mmr_by_age_df %>%
  full_join(overall_NJ, by = c(
    "county", "year", "age_of_mother", "maternal_deaths", "years_lived",
    "live_births", "mmrate", "mmrate_per_1kyl", "mmratio", "mmratio_per_100klb"
  )) %>%
  full_join(nj_ref %>% select("County_Name", "FIPS_Code"), ., by = c("County_Name" = "county"))

# save MMRate and MMRatio by Age to RDS file
saveRDS(mmr_by_age_df, "data/processed/mmr_by_age.rds")
