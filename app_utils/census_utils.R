library(assertthat)

#' Function to build the url to a specific county's race/ethnicity makeup for
#' ACS 1-Year Estimates / Table C03002
#'
#' @param year_4d - numeric four digit year between 2005 and 2017 from input$mm_slide_year2
#' @param fips_county - numeric fips county code for New Jersey state or county
#' @param geo_level - string level of geography (either county or state)
#'
#' @return A string representing the the right url to hyperlink to for that county
build_census_tbl_url <- function(year_4d, fips_county, geo_level) {
  base_url <- "https://data.census.gov/cedsci/table?q=C03002%3A%20HISPANIC%20OR%20LATINO%20ORIGIN%20BY%20RACE&table=DP05&tid=ACSDT1Y"
  mid_url <- ".C03002&g="
  end_url <- "&hidePreview=true&moe=true"

  geo_str <- ifelse(geo_level == "county", "0500000", "0400000")
  fips_county <- ifelse(geo_level == "state", "34", fips_county)

  proper_url <- paste0(base_url, year_4d, mid_url, geo_str, "US", fips_county, end_url)
  return(proper_url)
}
