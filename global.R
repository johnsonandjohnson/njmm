# Load Libraries
pacman::p_load(
  dplyr,
  leaflet,
  leaflet.extras,
  purrr,
  RColorBrewer,
  shiny,
  shinycssloaders,
  shinydashboard,
  shinylogs,
  shinyjs,
  stringr,
  tippy
)

# Source utility functions
source("app_utils/census_utils.R")
source("app_utils/color_utils.R")
source("app_utils/label_utils.R")
source("app_utils/leaflet_utils.R")
source("app_utils/methodology_utils.R")
source("app_utils/mmr_utils.R")
source("app_utils/plotly_utils.R")
source("app_utils/title_utils.R")

# Enabling Bookmarking
enableBookmarking(store = "url")

# Specify colors
COLORS_MMRATE <- "Greens"
COLORS_MMRATIO <- "Reds"
COLORS_UNINSURED <- "Blues"
COLORS_MEDICAID <- "Purples"
COLORS_CSECTION <- "RdBu"
COLORS_OBGYN <- "Oranges"
COLORS_MIDWIVES <- "RdPu"
COLORS_POVERTY <- "Blues"

# Processed Data Directory
p_data_dir <- file.path("data", "processed")

# Read in County Shape Files and Order Counties
COUNTIES <- readRDS(file.path(p_data_dir, "nj_counties_simplified.RDS"))
NJ_COUNTY_ORDER <- COUNTIES@data$FIPSCO %>%
  as.character() %>%
  as.integer() %>%
  order()

## MMRATIO and MMRATE
# Read in MMR file
MMR <- readRDS(file.path(p_data_dir, "mmr.rds"))


## MMRATIO and MMRATE - AGE OF MOTHER
# Read in MMR file
MMR_AGE <- readRDS(file.path(p_data_dir, "mmr_by_age.rds"))


## MMRATIO - ETHNICITY
# Read in MMRatio by ethnicity
MMRATIO_ETHNICITY <- readRDS(file.path(p_data_dir, "mmratio_by_race_ethnicity.rds"))


## UNIQUE GROUPS - used below in UI_GLOBALS for user input choices 
# Age Groups - MMRate
mmr_age_mmrate <- MMR_AGE %>%
  filter(!is.na(MMR_AGE$mmrate_per_1kyl))
mmRateUniqueAgeGroups <- unique(mmr_age_mmrate$age_of_mother)

# Age Groups - MMRatio
mmRatioUniqueAgeGroups <- unique(MMR_AGE$age_of_mother)

# Ethnicity Groups - MMRatio
mmRatioEthnicityUniqueGroups <- levels(MMRATIO_ETHNICITY$ethnicity_race)

# Unique Counties
mmUniqueCounties <- unique(MMR_AGE$County_Name) %>% sort()


## MAP LAYERS
# Specify layer names
layers <- list(
  pct_uninsured = "Percent Uninsured",
  pct_medicaid = "Percent Covered by Medicaid",
  ntsv_goal_diff = "Difference in NTSV Rate",
  per_capita_obgyns = "OB/GYN Physicians Per Capita",
  per_capita_midwives = "Practitioner Midwives Per Capita",
  pct_poverty = "Percent Poverty"
)

# 2016 Delivery Hospitals
BIRTH_HOSPS <- readRDS(file.path(p_data_dir, "hospitals_with_delivery.rds"))

# Read in other layer df
OTHER_LAYER_DF <- readRDS(file.path(p_data_dir, "other_layers.rds"))

# Grab max and/or min values for legend
MAX_PCT_UNINSURED_ANY <- max(OTHER_LAYER_DF$pct_uninsured)
MAX_PCT_INSURED_MEDICAID <- max(OTHER_LAYER_DF$pct_medicaid)
MAX_CSECTION <- max(OTHER_LAYER_DF$ntsv_goal_diff, na.rm = TRUE)
MIN_CSECTION <- min(OTHER_LAYER_DF$ntsv_goal_diff, na.rm = TRUE)
MAX_OBGYN <- max(OTHER_LAYER_DF$per_capita_obgyns, na.rm = TRUE)
MAX_MIDWIVES <- max(OTHER_LAYER_DF$per_capita_midwives, na.rm = TRUE)
MAX_POVERTY <- max(OTHER_LAYER_DF$percent_poverty, na.rm = TRUE)


# Maternal Mortality Measurements
MMRATE_PER_1KYL <- "mmrate_per_1kyl"
MMRATIO_PER_100KLB <- "mmratio_per_100klb"

# App Input Configuration - UI globals to avoid repeating code...
UI_GLOBALS <- list(
  
  # LaTeX Formulas
  MMRATIO_LATEX = "\\begin{equation}MMRatio = \\frac{Number\\,of\\,Maternal\\,Deaths}{Number\\,of\\,Live\\,Births} * 100,000\\end{equation}",
  MMRATE_LATEX = "\\begin{equation}MMRate = \\frac{Number\\,of\\,Maternal\\,Deaths}{Female\\,Population\\,Ages\\,15\\,to\\,49} * 1,000\\end{equation}",
  
  # Maternal Mortality Measurement Input Configuration
  MM_SELECT_LABEL = h5("Maternal Mortality Measure"),
  MM_SELECT_CHOICES = list(
    "Maternal Mortality Rate" = MMRATE_PER_1KYL,
    "Maternal Mortality Ratio" = MMRATIO_PER_100KLB
  ),
  MM_SELECT_DEFAULT = MMRATIO_PER_100KLB,

  # Demography Input Configuration
  MM_DEMO_LABEL = h5("Demographic Group"),
  MM_DEMO_CHOICES_MMRATIO = list(
    "All Women" = "all",
    "Age of Mother" = "age",
    "Race/Ethnicity" = "ethnicity"
  ),
  MM_DEMO_CHOICES_MMRATE = list(
    "All Women" = "all",
    "Age of Mother" = "age"
  ),
  MM_DEMO_DEFAULT = "all",
  MM_DEMO_RADIO_CHOICES_MMRATIO = list("Age of Mother" = "age", "Race/Ethnicity" = "ethnicity"),
  MM_DEMO_RADIO_CHOICES_MMRATE = list("Age of Mother" = "age"),
  MM_DEMO_RADIO_DEFAULT = "age",

  # Demography Group Input Choices
  MM_AGE_CHOICES_MMRATIO = mmRatioUniqueAgeGroups %>%
    prepend(list("All Ages" = "all_ages")),
  MM_AGE_CHOICES_MMRATE = mmRateUniqueAgeGroups %>%
    prepend(list("All Ages" = "all_ages")),
  MM_ETHNICITY_CHOICES_MMRATIO = mmRatioEthnicityUniqueGroups %>%
    prepend(list("All Ethnicities" = "all_ethnicities")),

  # Explore More County Selection
  MM_COUNTY_CHOICES = mmUniqueCounties,

  # Year Input Configuration
  MM_YEAR_LABEL = h3("Year"),
  MM_YEAR_MIN = 2005,
  MM_YEAR_MAX = 2017,
  MM_YEAR_DEFAULT = 2017,
  MM_YEAR_SEP = "",
  MM_YEAR_WIDTH = "800px",

  RESET_TIPPY_TEXT = "Resets all inputs to their defaults",
  RESET_TIPPY_PLACEMENT = "right"
)
