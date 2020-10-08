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

# Filter to NJ counties only
nj_ahrf <- ahrf_county %>%
  filter(F12424 == "NJ")

## Deeper dive into Ob-Gyns
obgyn_variables <- ahrf_county_layout %>%
  filter(grepl("Ob-Gyn, Gen, Total Patient Care", var_label))

# Grab the fips_code column and the columns that have the OBGYN data we want
obgyn_gen_total_patient_care_data <- nj_ahrf %>% select(F00002, F00010, all_of(obgyn_variables$field))

# Grab the years and make them the column names
new_year_column_names <- obgyn_variables$year_of_data
names(obgyn_gen_total_patient_care_data) <- c("FIPS_Code", "County_Name", new_year_column_names)

# Pivot into usable data frame
obgyn_gen_total_patient_care_data <- obgyn_gen_total_patient_care_data %>%
  pivot_longer(., cols = starts_with("2"), names_to = "year", values_to = "count") %>%
  mutate(
    count = as.double(count),
    year = as.numeric(year),
    FIPS_Code = as.numeric(FIPS_Code)
  ) %>%
  select(year, FIPS_Code, County_Name, count) # Reorder columns

## Get per capita counts
obgyns_per_capita <- per_capita(obgyn_gen_total_patient_care_data)
