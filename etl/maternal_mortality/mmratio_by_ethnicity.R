## Libraries
pacman::p_load(dplyr, forcats, janitor, purrr, readr, readxl, tidyr)

## Sources
source("./etl/maternal_mortality/maternal_deaths.R")

# NJ County Reference
nj_ref <- read_csv("data/reference/nj_county_reference.csv", col_types = cols()) %>%
  select(County_Name, FIPS_Code)

## Maternal Deaths Data
# Set mmratio folder where death data is stored
mmratio_file_path_prefix <- file.path("data", "raw", "nj_maternal_deaths", "mmratio_by_ethnicity")

# Get file names
maternal_death_files <- find_file_names(mmratio_file_path_prefix, "^Maternal Deaths 2005-2017 \\D+.xlsx")

# Set names so purrr can use them later
names(maternal_death_files) <- extract_attribute_groups(maternal_death_files)

# Read in and clean up maternal deaths
maternal_deaths <- maternal_deaths(maternal_death_files, mmratio_file_path_prefix, "ethnicity_race")

# Use regex to identify all the ethnicity/race files which end only in characters[a-z] then .xlsx
live_birth_files <- list.files(path = "data/raw/nj_live_births", pattern = "^Live Births 2005-2017 \\D+.xlsx")

# Set names so purrr can use them later
names(live_birth_files) <- live_birth_files %>%
  gsub("Live Births 2005-2017 ", "", .) %>%
  gsub("\\.xlsx", "", .)

# Map through all the files reading in the excel files and cleaning them up
# Then join them all together as a long dataframe
live_births <- live_birth_files %>%
  map(~ file.path("data/raw/nj_live_births", .x)) %>%
  map_dfr(~ read_excel(.x, skip = 10) %>% # Read in file
    slice(3:15) %>% # Only grab 2005-2017 data
    rename(year = `...1`) %>% # Fix year column name
    select(year, Atlantic:Warren) %>% # Only grab year + counties
    mutate_all(as.numeric) %>% # Fix types
    pivot_longer(-year,
      names_to = "county", # Pivot longer
      values_to = "live_births"
    ),
  .id = "ethnicity_race" # Append race of mother from the name of each element
  ) %>%
  select(year, county, ethnicity_race, live_births) %>% # Re-order columns
  mutate(ethnicity_race = as.factor(ethnicity_race) %>% fct_inorder())

# Calculate summaries of race/ethnicity mmratios for Overall NJ (All Counties of NJ)
mmratio_overall_NJ <- maternal_deaths %>%
  full_join(live_births, by = c("year", "county", "ethnicity_race")) %>%
  group_by(year, ethnicity_race) %>%
  summarise(
    maternal_deaths = sum(maternal_deaths, na.rm = TRUE),
    live_births = sum(live_births, na.rm = TRUE)
  ) %>%
  mutate(county = "All Counties of NJ") %>%
  ungroup()

# Calculate MMRatio
mmratio_df <- maternal_deaths %>%
  full_join(live_births, by = c("year", "county", "ethnicity_race")) %>%
  full_join(mmratio_overall_NJ, by = c("county", "year", "ethnicity_race", "maternal_deaths", "live_births")) %>%
  group_by(year, county, ethnicity_race) %>%
  summarise(
    maternal_deaths = sum(maternal_deaths, na.rm = TRUE),
    live_births = sum(live_births, na.rm = TRUE)
  ) %>%
  complete(year, county, ethnicity_race) %>%
  mutate(
    maternal_deaths = replace(maternal_deaths, is.na(maternal_deaths), 0),
    live_births = replace(live_births, is.na(live_births), 0),
    ethnicity_race = as.factor(ethnicity_race),
    mmratio = ifelse(live_births == 0, 0, maternal_deaths / live_births),
    mmratio_per_100klb = floor(mmratio * 100000),
    ethnicity_race = fct_relevel(factor(unique(.$ethnicity_race)), "Other", after = 4)
  ) %>%
  full_join(nj_ref, ., by = c("County_Name" = "county"))

saveRDS(mmratio_df, "data/processed/mmratio_by_race_ethnicity.rds")
