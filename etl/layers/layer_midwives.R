## This script should be executed after explore_mmratio_by_age_of_mother as we are using the woman-years
## lived as pop counts to determine per capita rate

## Libraries
pacman::p_load(
  dplyr,
  readr,
  readxl,
  tidyr
)

## Source
source("./etl/woman_years_lived.R")

## Read in AHRF
# Find start of the variable table in the technical documentation
bgn_line <- doc_file %>%
  read_excel() %>%
  pull(1) %>%
  grepl("F00001", .) %>%
  which()

# Generate fixed width/variable mappings
ahrf_county_layout <- doc_file %>%
  read_excel(
    skip = bgn_line - 1,
    col_names = c(
      "field", "col_col", "year_of_data", "var_label",
      "characteristics", "source", "date_on"
    )
  ) %>%
  filter(grepl("^F[0-9]", field)) %>%
  separate(col_col, c("col_start", "col_end")) %>%
  mutate_at(c("col_start", "col_end"), as.integer)

# Read in data file with fixed width/variable mappings
ahrf_county <- data_file %>%
  read_fwf(fwf_positions(
    start = ahrf_county_layout$col_start,
    end = ahrf_county_layout$col_end,
    col_names = ahrf_county_layout$field
  ),
  col_types = cols()
  )

# Midwives
midwife_cols <- paste0("F14645-", 10: substring(first(AHRF_YEARS)-1, 3))

# Filter to NJ counties only
midwife_counts <- ahrf_county %>%
  filter(F12424 == "NJ") %>%
  select(c("F00010", "F00012", all_of(midwife_cols))) %>%
  pivot_longer(
    cols = matches("F14645"),
    names_to = "year",
    names_prefix = "F14645-",
    values_to = "count"
  ) %>%
  mutate(
    year = as.numeric(paste0("20", year)),
    count = as.numeric(count),
    FIPS_Code = as.numeric(paste0("34", F00012)),
    County_Name = F00010
  ) %>%
  select(
    FIPS_Code,
    County_Name,
    year,
    count
  ) %>%
  arrange(FIPS_Code)

## Get per capita counts
midwives_per_capita <- per_capita(midwife_counts)
