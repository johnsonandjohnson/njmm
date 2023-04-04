rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/mmr_utils.R")

mmr_utils_path_prefix <- file.path(path_prefix, "mmr_utils_test")
validated_mmr_utils_path_prefix <- file.path(path_prefix, "mmr_utils_test", "validated")

# Set the variables that will be returned by the mmr_df function, based off of the parameters given (test_mmr_utils.R)
MMR <<- readRDS(file.path(mmr_utils_path_prefix, "test_mmr.rds"))
MMR_AGE <<- readRDS(file.path(mmr_utils_path_prefix, "test_mmr_by_age.rds"))
MMRATIO_ETHNICITY <<- readRDS(file.path(mmr_utils_path_prefix, "test_mmratio_by_race_ethnicity.rds"))


## MMRATIO
test_that(
  "Testing mmrate for all women of all ages",
  expect_equal(
    mmr_df(TRUE, "all", NULL),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_all.RDS"))
  )
)

# AGE
test_that(
  "Testing mmrate for all women of all ages",
  expect_equal(
    mmr_df(TRUE, "age", "all_ages"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_all.RDS"))
  )
)

test_that(
  "Testing mmrate for all women of ages 20-24",
  expect_equal(
    mmr_df(TRUE, "age", "20-24"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_age_20-24.RDS"))
  )
)

test_that(
  "Testing mmrate for all women of ages Over 50",
  expect_equal(
    mmr_df(TRUE, "age", "Over 50"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_age_over50.RDS"))
  )
)

# ETHNICITY
test_that(
  "Testing mmrate for all women of all ethnicities",
  expect_equal(
    mmr_df(TRUE, "ethnicity", "all_ethnicities"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_all.RDS"))
  )
)

test_that(
  "Testing mmrate for all women of Other Ethnicity",
  expect_equal(
    mmr_df(TRUE, "ethnicity", "Other"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmratio_ethnicity_other.RDS"))
  )
)

## MMRATE
test_that(
  "Testing mmrate for all women",
  expect_equal(
    mmr_df(FALSE, "all", NULL),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_all.RDS"))
  )
)

# AGE
test_that(
  "Testing mmrate for all women of all ages",
  expect_equal(
    mmr_df(FALSE, "age", "all_ages"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmr_all.RDS"))
  )
)

test_that(
  "Testing mmrate for all women of ages 45-49",
  expect_equal(
    mmr_df(FALSE, "age", "45-49"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmrate_age_45-49.RDS"))
  )
)


### Tests for mmr_df_explore_more()
## MMRATIO
# AGE
test_that(
  "Testing explore more data for mmratio in age demographic of Cape May county for 2017 (0)",
  expect_equal(
    mmr_df_explore_more("mmratio_per_100klb", 2017, "age", "Cape May"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmratio_age_capemay_2017.RDS"))
  )
)

test_that(
  "Testing explore more data for mmratio in age demographic of Bergen county for 2017",
  expect_equal(
    mmr_df_explore_more("mmratio_per_100klb", 2017, "age", "Bergen"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmratio_age_bergen_2017.RDS"))
  )
)

# ETHNICITY
test_that(
  "Testing explore more data for mmratio in ethnicity demographic of Cumberland county for 2010",
  expect_equal(
    mmr_df_explore_more("mmratio_per_100klb", 2010, "ethnicity", "Cumberland"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmratio_ethnicity_cumberland_2010.RDS"))
  )
)

## MMRATE
# AGE
test_that(
  "Testing explore more data for mmrate in age demographic of Bergen county for 2017",
  expect_equal(
    mmr_df_explore_more("mmrate_per_1kyl", 2017, "age", "Bergen"),
    read_rds(file.path(validated_mmr_utils_path_prefix, "mmrate_age_bergen_2017.RDS"))
  )
)

