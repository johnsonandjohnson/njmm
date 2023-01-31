rprojroot::find_root("nj_mmr_pilot.Rproj") %>%
  setwd()
source("./app_utils/title_utils.R")

### Tests for build_map_title()
## MMRATIO
# 2017 Maternal Mortality Ratio by County
test_that(
  "Testing '2017 Maternal Mortality Ratio by County' title",
  expect_equal(
    build_map_title(2017, "mmratio_per_100klb", "all", NULL),
    "2017 Maternal Mortality Ratio by County" %>% HTML()
  )
)

# 2017 Maternal Mortality Ratio by County for Women of All Ages
test_that(
  "Testing '2017 Maternal Mortality Ratio by County for Women of All Ages' title",
  expect_equal(
    build_map_title(2017, "mmratio_per_100klb", "age", "all_ages"),
    "2017 Maternal Mortality Ratio by County</br>for Women of All Ages" %>% HTML()
  )
)

# 2017 Maternal Mortality Ratio by County for Women of All Ethnicities
test_that(
  "Testing '2017 Maternal Mortality Ratio by County for Women of All Ethnicities' title",
  expect_equal(
    build_map_title(2017, "mmratio_per_100klb", "ethnicity", "all_ethnicities"),
    "2017 Maternal Mortality Ratio by County</br>for Women of All Ethnicities" %>% HTML()
  )
)

# 2017 Maternal Mortality Ratio by County for Women of Ages 15-19
test_that(
  "Testing '2017 Maternal Mortality Ratio by County for Women of Ages 15-19' title",
  expect_equal(
    build_map_title(2017, "mmratio_per_100klb", "age", "15-19"),
    "2017 Maternal Mortality Ratio by County</br>for Women of Ages 15-19" %>% HTML()
  )
)

# 2017 Maternal Mortality Ratio by County for Women of Ages Over 50
test_that(
  "Testing '2017 Maternal Mortality Ratio by County for Women of Ages Over 50' title",
  expect_equal(
    build_map_title(2017, "mmratio_per_100klb", "age", "Over 50"),
    "2017 Maternal Mortality Ratio by County</br>for Women of Ages Over 50" %>% HTML()
  )
)

# 2017 Maternal Mortality Ratio by County for Women of Black Non-Hispanic Ethnicity
test_that(
  "Testing '2017 Maternal Mortality Ratio by County for Women of Black Non-Hispanic Ethnicity' title",
  expect_equal(
    build_map_title(2017, "mmratio_per_100klb", "ethnicity", "Black Non-Hispanic"),
    "2017 Maternal Mortality Ratio by County</br>for Women of Black Non-Hispanic</br>Ethnicity" %>% HTML()
  )
)


## MMRATE
# 2017 Maternal Mortality Rate by County
test_that(
  "Testing '2017 Maternal Mortality Rate by County' title",
  expect_equal(
    build_map_title(2017, "mmrate_per_1kyl", "all", NULL),
    "2017 Maternal Mortality Rate by County" %>% HTML()
  )
)

# 2017 Maternal Mortality Rate by County for Women of All Ages
test_that(
  "Testing '2017 Maternal Mortality Rate by County for Women of All Ages' title",
  expect_equal(
    build_map_title(2017, "mmrate_per_1kyl", "age", "all_ages"),
    "2017 Maternal Mortality Rate by County</br>for Women of All Ages" %>% HTML()
  )
)

# 2017 Maternal Mortality Rate by County for Women of All Ethnicities
test_that(
  "Testing '2017 Maternal Mortality Rate by County for Women of All Ethnicities' title",
  expect_equal(
    build_map_title(2017, "mmrate_per_1kyl", "ethnicity", "all_ethnicities"),
    "2017 Maternal Mortality Rate by County</br>for Women of All Ethnicities" %>% HTML()
  )
)

# 2017 Maternal Mortality Rate by County for Women of Ages 45-49
test_that(
  "Testing '2017 Maternal Mortality Rate by County for Women of Ages 45-49' title",
  expect_equal(
    build_map_title(2017, "mmrate_per_1kyl", "age", "45-49"),
    "2017 Maternal Mortality Rate by County</br>for Women of Ages 45-49" %>% HTML()
  )
)

# 2017 Maternal Mortality Rate by County for Women of Other Ethnicity
test_that(
  "Testing '2017 Maternal Mortality Rate by County for Women of Other' title",
  expect_equal(
    build_map_title(2017, "mmrate_per_1kyl", "ethnicity", "Other"),
    "2017 Maternal Mortality Rate by County</br>for Women of Other</br>Ethnicity" %>% HTML()
  )
)


### Tests for build_layer_map_title()
## Percent Uninsured
# 2017 Percent Uninsured by County for Females Ages 6-54
test_that(
  "Testing '2017 Percent Uninsured by County for Females Ages 6-54' title",
  expect_equal(
    build_layer_map_title(2017, "Percent Uninsured"),
    "2017 Percent Uninsured by County</br>for Females Ages 6-54" %>% HTML()
  )
)

## Percent Covered by Medicaid
# 2016 Percent Covered by Medicaid by County for Females Ages 6-54
test_that(
  "Testing '2016 Percent Covered by Medicaid by County for Females Ages 6-54' title",
  expect_equal(
    build_layer_map_title(2016, "Percent Covered by Medicaid"),
    "2016 Percent Covered by Medicaid by County</br>for Females Ages 6-54" %>% HTML()
  )
)

## Difference in NTSV Rate
# 2015 Difference in NTSV Cesarean Rate from 2020 NJ Statewide Goal by County
test_that(
  "Testing '2015 Difference in NTSV Cesarean Rate from 2020 NJ Statewide Goal by County' title",
  expect_equal(
    build_layer_map_title(2015, "Difference in NTSV Rate"),
    "2015 Difference in NTSV Cesarean Rate</br>from 2020 NJ Statewide Goal by County" %>% HTML()
  )
)

## OB/GYN Physicians Per Capita
# 2014 OB/GYN Physicians Per Capita by County (with info about why it's showing year of 2010)
test_that(
  "Testing that the title '2014 OB/GYN Physicians Per Capita by County' gets changed to
  '2010 OB/GYN Physicians Per Capita by County' since no data is available for 2014",
  expect_equal(
    build_layer_map_title(2014, "OB/GYN Physicians Per Capita"),
    '2010 OB/GYN Physicians Per Capita by County <br/> <i class=\"fas fa-circle-exclamation\" role=\"presentation\" aria-label=\"circle-exclamation icon\"></i> <i>Years differ due to data availability.</i>' %>% HTML()
  )
)

# 2015 OB/GYN Physicians Per Capita by County
test_that(
  "Testing '2015 OB/GYN Physicians Per Capita by County' title",
  expect_equal(
    build_layer_map_title(2015, "OB/GYN Physicians Per Capita"),
    "2015 OB/GYN Physicians Per Capita by County" %>% HTML()
  )
)

## Practitioner Midwives Per Capita
# 2013 Practitioner Midwives Per Capita by County
test_that(
  "Testing '2013 Practitioner Midwives Per Capita by County' title",
  expect_equal(
    build_layer_map_title(2013, "Practitioner Midwives Per Capita"),
    "2013 Practitioner Midwives Per Capita by County" %>% HTML()
  )
)

## Percent Poverty
# 2012 Percent Poverty by County
test_that(
  "Testing '2012 Percent Poverty by County' title",
  expect_equal(
    build_layer_map_title(2012, "Percent Poverty"),
    "2012 Percent Poverty by County" %>% HTML()
  )
)


### Tests for build_graph_title
## MMRATIO
# Maternal Mortality Ratios in Bergen County were all 0 for 2016
test_that(
  "Testing 'Maternal Mortality Ratios in Bergen County were all 0 for 2016' title",
  expect_equal(
    build_graph_title(TRUE, 2016, "mmratio_per_100klb", "Bergen"),
    "Maternal Mortality Ratios in Bergen County were all 0 for 2016" %>% HTML()
  )
)

# 2010 Maternal Mortality Ratios for Bergen County
test_that(
  "Testing '2010 Maternal Mortality Ratios for Bergen County' title",
  expect_equal(
    build_graph_title(FALSE, 2010, "mmratio_per_100klb", "Bergen"),
    "2010 Maternal Mortality Ratios for Bergen County" %>% HTML()
  )
)


## MMRATE
# Maternal Mortality Rates in Cape May County were all 0 for 2017
test_that(
  "Testing 'Maternal Mortality Rates in Cape May County were all 0 for 2017' title",
  expect_equal(
    build_graph_title(TRUE, 2017, "mmrate_per_1kyl", "Cape May"),
    "Maternal Mortality Rates in Cape May County were all 0 for 2017" %>% HTML()
  )
)

# 2005 Maternal Mortality Rates for Cape May County
test_that(
  "Testing '2005 Maternal Mortality Rates for Cape May County' title",
  expect_equal(
    build_graph_title(FALSE, 2005, "mmrate_per_1kyl", "Cape May"),
    "2005 Maternal Mortality Rates for Cape May County" %>% HTML()
  )
)
