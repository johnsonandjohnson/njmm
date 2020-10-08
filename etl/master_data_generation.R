# Use this script to generate all RDS files from raw data for the application

maternal_mortality_prefix <- file.path("./etl/maternal_mortality")
layer_prefix <- file.path("./etl/layers")

# MMRatio and MMRate RDS files
source(file.path(maternal_mortality_prefix, "mmr.R"))
source(file.path(maternal_mortality_prefix, "mmr_by_age", "combine_mmr_by_age_of_mother.R"))
source(file.path(maternal_mortality_prefix, "mmratio_by_ethnicity.R"))

# Layer RDS files
source(file.path(layer_prefix, "combine_layers.R"))
source(file.path(layer_prefix, "hospitals_with_delivery.R"))
