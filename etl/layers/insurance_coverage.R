## Libraries
pacman::p_load(dplyr, janitor, purrr, readr)


#' Function to read in specified insurance files from years 2010
#' to 2018 that cleans the data and computes percent insured and
#' uninsured for that particular insurance, then saves df to RDS
#'
#' @param insurance_type: the type of insurance
#' -- Options: "Medicaid", or "Health" (Any insurance)
percent_insured_to_RDS <- function(insurance_type) {

  # Read in ACS data
  acs_files <- file.path("data", "raw", paste0("acs_", tolower(insurance_type), "_insurance")) %>%
    list.files(".csv", full.names = T) %>%
    setNames(2010:2018)

  insurance_cov <- acs_files %>%
    map(read_csv, skip = 1, col_types = cols()) %>%
    map_dfr(clean_names, .id = "year")


  # Clean it up a bit and downselect to what we want
  insurance_cov <- insurance_cov %>%
    select(
      year, id,
      contains("Female"),
      -contains("margin"),
      -contains("under_6"),
      -contains("to_64"),
      -contains("to_74"),
      -contains("75_years")
    ) %>%
    rename(FIPS_Code = id) %>%
    mutate(FIPS_Code = gsub("0500000US", "", FIPS_Code))


  # Get % of females aged 6-54 with any insurance within each county
  total_pop <- insurance_cov %>%
    select(ends_with("years")) %>%
    transmute(all_females = rowSums(., na.rm = T)) %>%
    bind_cols(insurance_cov %>% select(year, FIPS_Code), .)

  with_ins <- insurance_cov %>%
    select(contains(paste0("with_", tolower(insurance_type)))) %>%
    transmute(with_insurance = rowSums(., na.rm = T)) %>%
    bind_cols(insurance_cov %>% select(year, FIPS_Code), .)

  no_ins <- insurance_cov %>%
    select(contains(paste0("no_", tolower(insurance_type)))) %>%
    transmute(no_insurance = rowSums(., na.rm = T)) %>%
    bind_cols(insurance_cov %>% select(year, FIPS_Code), .)

  cov_df <- total_pop %>%
    left_join(with_ins, by = c("year", "FIPS_Code")) %>%
    left_join(no_ins, by = c("year", "FIPS_Code")) %>%
    mutate(year = as.numeric(year)) %>%
    mutate_at(
      .vars = vars(with_insurance, no_insurance),
      .funs = funs(`pct` = round((. / all_females) * 100, 1))
    )
}
