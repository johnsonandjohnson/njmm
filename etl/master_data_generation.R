# Use this script to generate all RDS files from raw data for the application
# Set years of data 
YEARS_OF_DATA <- c(2005:2019)
first_year <- first(YEARS_OF_DATA)
last_year <- last(YEARS_OF_DATA)
dates <- paste(first_year, last_year, sep = "-")

maternal_mortality_prefix <- file.path("./etl/maternal_mortality")
layer_prefix <- file.path("./etl/layers")

# MMRatio and MMRate RDS files
source(file.path(maternal_mortality_prefix, "mmr.R"))
source(file.path(maternal_mortality_prefix, "mmr_by_age", "combine_mmr_by_age_of_mother.R"))
source(file.path(maternal_mortality_prefix, "mmratio_by_ethnicity.R"))

# Layer RDS files
INSURANCE_YEARS_OF_DATA <- c(2010:2019)
NTSV_YEARS <- "2000-2020"
AHRF_YEARS <- c(2020:2021)
source(file.path(layer_prefix, "combine_layers.R"))
source(file.path(layer_prefix, "hospitals_with_delivery.R"))
