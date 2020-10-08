## Sources
source("./etl/layers/insurance_coverage.R")

insurance_cov_any <- percent_insured_to_RDS("Health") %>%
  select(year, FIPS_Code, no_insurance_pct)

insurance_cov_medicaid <- percent_insured_to_RDS("Medicaid") %>%
  select(year, FIPS_Code, with_insurance_pct)
