rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./etl/woman_years_lived.R")
local_edition(3)

# Set the file path to point to the data for these tests
woman_years_lived_path_prefix <- file.path(path_prefix, "woman_years_lived_test")

# Test for calculate_woman_years_lived function for 15 to 49 years
test_ACS_pop_data <- file.path(woman_years_lived_path_prefix, "test_acs_data.csv") %>% read_csv(col_types = cols())
validated_data_15_49 <- file.path(
  woman_years_lived_path_prefix,
  "test_calculate_woman_years_lived_validated_15_49.csv"
) %>%
  read_csv(col_types = cols())
test_ACS_pop_data_15_49 <- calculate_woman_years_lived(test_ACS_pop_data, 15, 49)
test_ACS_pop_data_15_53 <- calculate_woman_years_lived(test_ACS_pop_data, 15, 53)

test_that(
  "Verifying that woman-years lived has been calculated as expected for age groups 15 to 49",
  expect_equal(test_ACS_pop_data_15_49, validated_data_15_49, ignore_attr = TRUE)
)

test_that(
  "Verifying that woman-years lived has been calculated as expected for age groups 15 to 49
  as 53 falls into the middle of the next age group and therefore should not be included",
  expect_equal(test_ACS_pop_data_15_53, validated_data_15_49, ignore_attr = TRUE)
)

# Test for calculate_woman_years_lived function for 18 to 54 years
validated_data_18_54 <- file.path(
  woman_years_lived_path_prefix,
  "test_calculate_woman_years_lived_validated_18_54.csv"
) %>%
  read_csv(col_types = cols())
test_ACS_pop_data_16_54 <- calculate_woman_years_lived(test_ACS_pop_data, 16, 54)

test_that(
  "Verifying that woman-years lived has been calculated as expected for age groups 18 to 54
  as 16 falls within the middle of the previous age group and therefore should not be included",
  expect_equal(test_ACS_pop_data_16_54, validated_data_18_54, ignore_attr = TRUE)
)

wyl_age_groups_col_types <- cols(
  fips_code = col_double(),
  age_of_mother = col_factor(),
  year = col_double(),
  years_lived = col_double()
)

# Test for wyl_age_groups
test_wyl_age_group <- wyl_age_group(15, 19)
validated_wyl_age_group <- read_csv(file.path(woman_years_lived_path_prefix, "test_wyl_age_group_validated.csv"),
  col_types = wyl_age_groups_col_types
)

test_that(
  "Verifying that wyl_age_group() returns a tibble showing the WYL for age group 15-17
  over the years 2005-2017",
  expect_equal(test_wyl_age_group, validated_wyl_age_group, ignore_attr = TRUE)
)
