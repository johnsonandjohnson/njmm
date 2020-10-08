pacman::p_load(readr, testit, tidyr)

# ACS Population Data File Path for 2 digit year YY
acs_pop_path <- file.path("data", "raw", "acs_population", "ACS_YY_1YR_B01001_with_ann.csv")

# Grabbing multiplier for each age group
woman_years_multiplier <- read_delim("./etl/maternal_mortality/years_lived_multiplier.txt", "|", col_types = cols())

# Years we're working with
years <- c(2005:2017)

#' Function to read in ACS Population data for a specific year,
#' clean it, and calculate the woman-years lived (for MMRate) for that year
#'
#' @param year: the four digit year to grab the ACS data on
#' @param min_age: the minimum age used to calculate WYL passed onto the
#' child function
#' @param max_age: the maximum age used to calculate WYL passed onto the
#' child function
#'
#' @return: a tibble with the FIPS_Code that pertains to the county
#' along with the woman years lived for each county
woman_years_lived <- function(year, min_age = 15, max_age = 49) {
  assert("Test four digits between 2005 and 2017", {
    x <- year
    (x >= first(years))
    (x <= last(years))
  })

  # Replace 2 digit year value of csvPath with the year we want
  acs_pop_path <- gsub("YY", substr(year, 3, 4), acs_pop_path)

  # Read in data
  nj_pop <- read_csv(acs_pop_path, skip = 1, col_types = cols())

  # Clean up messy ACS data and select only female population
  nj_pop <- nj_pop %>%
    select(starts_with("Estimate; Female")) %>%
    rename_all(~ gsub("Estimate; Female: - ", "", .)) %>%
    select(-1) %>%
    clean_names() %>%
    mutate(FIPS_Code = nj_pop$Id2) %>%
    select(FIPS_Code, everything())

  # Calculate woman years-lived 15-49 using the median of each age range and return it
  return(calculate_woman_years_lived(nj_pop, min_age, max_age))
}

#' Given tibble of ACS Pop data, calculates woman-years-lived
#' for a specified age range by pulling data for one or more ACS age groups
#' (depending on min and max parameters) from ACS Population data, and
#' aggregating all ACS age groups for each county to return the woman years
#' lived (or population for when num_years = 1) for that age range
#'
#' @param ACS_data: the ACS data year aggregates to calculate over
#' @param min_age: the minimum age used to calculate WYL
#' @param max_age: the maximum age used to calculate WYL
#' @param num_years: the number of years used to calculate person years lived,
#' currently defaulting to 1 as we aren't set up fully to do more than 1 year
#' calculations (need to account for WYL for those who are now over 49)
#'
#' @return: a tibble with the FIPS_Code that pertains to the county
#' along with the woman years lived for each county
calculate_woman_years_lived <- function(ACS_data, min_age, max_age, num_years = 1) {
  woman_years_multiplier <- woman_years_multiplier %>%
    filter(Max_Age <= max_age, Min_Age >= min_age)

  years_lived <- ACS_data %>%
    pivot_longer(., cols = starts_with(c("x", "under"))) %>%
    right_join(woman_years_multiplier, by = c("name" = "ACS Col")) %>%
    clean_names() %>%
    mutate(years_lived = min(years_lived_multiplier, num_years) * value) %>%
    select(fips_code, years_lived) %>%
    group_by(fips_code) %>%
    summarise(., years_lived = sum(years_lived))

  return(years_lived)
}

#' Calculates WYL over all years for a particular age group
#'
#' @param min: the min age of the age group
#' @param max: the max age of the age group
#'
#' @return: a tibble with fips_code, age_of_mother, year, and years_lived
wyl_age_group <- function(min, max) {
  wyl <- list(years, min, max) %>%
    pmap(woman_years_lived) %>%
    reduce(full_join, by = "fips_code") %>%
    mutate(age_of_mother = ifelse(min != max, paste(min, max, sep = "-"), min)) %>%
    setNames(., c("fips_code", years, "age_of_mother")) %>%
    pivot_longer(., cols = starts_with("2"), names_to = "year", values_to = "years_lived") %>%
    mutate(
      year = as.numeric(year),
      age_of_mother = as.factor(age_of_mother)
    )

  return(wyl)
}

#' Takes in a df of counts (midwives, or obgyn) and returns per capita rates
#' based off of population age groups used for woman years lived in mmrate
#' calculations (population of women ages 15-49) along with an extra age group
#' over 50
#'
#' @param counts_dataframe: the dataframe of the counts of each county and year
#'
#' @return: a tibble with fips_code, county_name, count, population, per_capita_rate
#' and per_capita_100K
per_capita <- function(counts_dataframe) {

  # Grab population data
  mmRateAge <- readRDS("data/processed/mmr_by_age.rds") %>%
    clean_names() %>%
    ungroup() %>%
    na.omit(NA) %>%
    select(year, fips_code, age_of_mother, years_lived)

  if (exists("mmRateAge")) {
    per_capita <- wyl_age_group(50, 74) %>%
      bind_rows(mmRateAge) %>%
      group_by(year, fips_code) %>%
      summarise(population = sum(years_lived)) %>%
      right_join(counts_dataframe, by = c("year", "fips_code" = "FIPS_Code")) %>%
      mutate(
        per_capita_rate = as.double(count / population),
        per_capita_100K = round(per_capita_rate * 100000, 3),
        FIPS_Code = as.character(fips_code)
      ) %>%
      select(-fips_code) %>%
      arrange(
        FIPS_Code,
        year,
        count,
        population,
        per_capita_rate,
        per_capita_100K
      )
    return(per_capita)
  }
}
