pacman::p_load(dplyr, readr, readxl, tidyr)

## Read in AHRF
# (adapted from https://github.com/jjchern/ahrf/blob/master/data-raw/prep_county.R)

doc_file <- "data/AHRF_2018-2019/DOC/AHRF 2018-2019 Technical Documentation.xlsx"
data_file <- "data/AHRF_2018-2019/DATA/AHRF2019.asc"

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

ahrf_county

# Filter to NJ counties only
nj_ahrf <- ahrf_county %>%
  filter(F12424 == "NJ")

## Potentially relevant variables
# Ob-Gyns
ahrf_county_layout %>%
  filter(grepl("Ob-Gyn", var_label))

# Midwives
ahrf_county_layout %>%
  filter(grepl("Midwives", var_label))

# Obstetric
ahrf_county_layout %>%
  filter(grepl("Obstetric", var_label))
