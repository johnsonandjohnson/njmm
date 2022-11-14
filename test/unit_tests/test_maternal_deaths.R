rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./etl/maternal_mortality/maternal_deaths.R")

maternal_deaths_path_prefix <- file.path(path_prefix, "maternal_deaths_test", "test_maternal_deaths")
age_groups <- c("15-19", "20-24")
dates <- paste(2005, 2017, sep = "-")

# Test for find_file_names
test_find_file_names <- find_file_names(maternal_deaths_path_prefix, "^Maternal Deaths.*[0-9]{2}\\.xlsx")
validated_file_names <- unlist(map(age_groups, ~ paste0("Maternal Deaths ", dates, " ", .x, ".xlsx")))

test_that(
  "Verifying that find_file_names() gives us the same files as expected",
  expect_equal(test_find_file_names, validated_file_names)
)

# Test for extract_attribute_groups
test_extract_attribute_age_groups <- extract_attribute_groups(validated_file_names, dates)
validated_age_groups <- age_groups

test_that(
  "Verifying that extract_attribute_groups() gives us the same age groups as expected",
  expect_equal(test_extract_attribute_age_groups, validated_age_groups)
)

# Test for maternal_deaths
names(validated_file_names) <- validated_age_groups
test_maternal_deaths <- maternal_deaths(validated_file_names, maternal_deaths_path_prefix, "age_group")
validated_maternal_deaths <- file.path(path_prefix, "maternal_deaths_test", "test_maternal_deaths_validated.csv") %>%
  read_csv(col_types = "icfd")

test_that(
  "Verifying that maternal_deaths() gives us the same tibble as expected",
  expect_equal(test_maternal_deaths %>% as.data.frame(),
    validated_maternal_deaths %>% as.data.frame(),
    check.attributes = FALSE
  )
)
