## Libraries
pacman::p_load(dplyr, forcats, janitor, purrr, readr, readxl, tidyr)

## Sources
source("./etl/maternal_mortality/maternal_deaths.R")

## Maternal Deaths Data
# Set mmratio folder where death data is stored
mmratio_file_path_prefix <- file.path("data", "raw", "nj_maternal_deaths", "mmratio_by_age")

# Get file names
maternal_death_files <- find_file_names(mmratio_file_path_prefix, "^Maternal Deaths.*[0-9]{2}\\.xlsx")

# Set names so purrr can use them later
# dates variable set in master generation script
names(maternal_death_files) <- extract_attribute_groups(maternal_death_files, dates)

# Read in and clean up maternal deaths
maternal_deaths <- maternal_deaths(maternal_death_files, mmratio_file_path_prefix, "age_of_mother")

## Live Births Data
# Use regex to identify all the age files which end in two digit number then .xlsx
live_birth_files <- list.files(path = "data/raw/nj_live_births", pattern = "^Live Births.*[0-9]{2}\\.xlsx")

# Set names so purrr can use them later
names(live_birth_files) <- live_birth_files %>%
  gsub(paste0("Live Births ", dates, " "), "", .) %>%
  gsub("\\.xlsx", "", .)

# Map through all the files reading in the excel files and cleaning them up
# Then join them all together as a long dataframe
live_births <- live_birth_files %>%
  map(~ file.path("data/raw/nj_live_births", .x)) %>%
  map_dfr(~ read_excel(.x, skip = 10) %>% # Read in file
    slice(3:(length(YEARS_OF_DATA)+2)) %>% # Only grab year summaries up top
    rename(year = `...1`) %>% # Fix year column name
    select(year, Atlantic:Warren) %>% # Only grab year + counties
    mutate_all(as.numeric) %>% # Fix types
    pivot_longer(-year,
      names_to = "county", # Pivot longer
      values_to = "live_births"
    ),
  .id = "age_of_mother" # Append age of mother from the name of each element
  ) %>%
  select(year, county, age_of_mother, live_births) %>% # Re-order columns
  mutate(age_of_mother = as.factor(age_of_mother) %>% fct_inorder())
# Make age of mother a properly ordered factor so the visualization later is nice

# Calculate MMRatio
mmratio_df <- maternal_deaths %>%
  full_join(live_births, by = c("year", "county", "age_of_mother")) %>%
  group_by(year, county, age_of_mother) %>%
  summarise(
    maternal_deaths = sum(maternal_deaths, na.rm = TRUE),
    live_births = sum(live_births, na.rm = TRUE)
  )%>% 
  ungroup %>% 
  complete(year, county, age_of_mother) %>% 
  mutate(
    maternal_deaths = replace(maternal_deaths, is.na(maternal_deaths), 0),
    live_births = replace(live_births, is.na(live_births), 0),
    mmratio = ifelse(live_births == 0, 0, maternal_deaths / live_births),
    mmratio_per_100klb = floor(mmratio * 100000)
  )

mmratio_df
