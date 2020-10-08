pacman::p_load(
  dplyr,
  readr,
  readxl,
  tidyr
)

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

# Poverty columns
poverty_cols <- paste0("F13321-", 10:17)

# Filter to NJ counties only
poverty_percent <- ahrf_county %>%
  filter(F12424 == "NJ") %>%
  select(c("F00010", "F00012", all_of(poverty_cols))) %>%
  pivot_longer(
    cols = matches("F13321"),
    names_to = "year",
    names_prefix = "F13321-",
    values_to = "percent_poverty"
  ) %>%
  mutate(
    year = paste0("20", year) %>% as.numeric(),
    percent_poverty = as.numeric(percent_poverty) * 0.1,
    FIPS_Code = paste0("34", F00012)
  ) %>%
  select(
    FIPS_Code,
    year,
    percent_poverty
  ) %>%
  arrange(FIPS_Code)
