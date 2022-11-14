## Libraries
pacman::p_load(dplyr, forcats, janitor, purrr, readxl)

## Sources
source("./etl/woman_years_lived.R")
source("./etl/maternal_mortality/maternal_deaths.R")

## Maternal Deaths Data
# Set mmrate folder where death data is stored
mmrate_file_path_prefix <- file.path("data", "raw", "nj_maternal_deaths", "mmrate_by_age")

# Get file names
maternal_death_files <- find_file_names(mmrate_file_path_prefix, "^Maternal Deaths.*[0-9]{2}\\.xlsx")

# Set names so purrr can use them later
# dates variable set in master generation script
names(maternal_death_files) <- extract_attribute_groups(maternal_death_files, dates)

# Read in and clean up maternal deaths
maternal_deaths <- maternal_deaths(maternal_death_files, mmrate_file_path_prefix, "age_of_mother")

## Woman years lived for each age group of each year
# Take the age ranges from the excel file names, and put them into a df with min and max of age groups
age_range <- names(maternal_death_files)
age_groups <- data.frame(age_range) %>%
  tidyr::separate(age_range, c("min", "max"), sep = "-") %>%
  mutate(max = map2(.$max, .$min, replace_na))

woman_years_lived_by_age <- age_groups %>%
  pmap(wyl_age_group) %>%
  reduce(full_join, by = c("fips_code", "year", "age_of_mother", "years_lived")) %>%
  left_join(nj_ref %>% select(FIPS_Code, County_Name), by = c("fips_code" = "FIPS_Code")) %>%
  rename(county = County_Name) %>%
  mutate(year = as.numeric(year))


# Calculate MMRate
mmrate_df <- maternal_deaths %>%
  full_join(woman_years_lived_by_age, by = c("county", "year", "age_of_mother")) %>%
  group_by(year, county, age_of_mother) %>%
  summarise(
    maternal_deaths = sum(maternal_deaths, na.rm = TRUE),
    years_lived = sum(years_lived, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  complete(year, county, age_of_mother) %>%
  mutate(
    maternal_deaths = replace(maternal_deaths, is.na(maternal_deaths), 0),
    years_lived = replace(years_lived, is.na(years_lived), 0),
    age_of_mother = as.factor(age_of_mother) %>% fct_inorder(),
    mmrate = ifelse(years_lived == 0, 0, maternal_deaths / years_lived),
    mmrate_per_1kyl = round(mmrate * 1000, 3)
  )
