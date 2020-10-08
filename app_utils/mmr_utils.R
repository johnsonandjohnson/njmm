#' Function to determine which maternal mortality data frame (sourced from global.R) should be used based on
#' the user's input values - data frame options: MMR, MMR_AGE, and MMRATIO_ETHNICITY
#'
#' @param mmratio - whether or not it's mmratio
#' @param demography_selected - the user's selected demography (derived from input$demography_select_box)
#' @param demography_group_selected - the specific age or ethnicity group pertaining
#' to the demography_selected (derived from input$age_select_box or input$demography_select_box)
#'
#' @return A dataframe pertaining to the user's selected crtiera
mmr_df <- function(mmratio, demography_selected, demography_group_selected) {

  # If user hasn't selected a demography group, return overall MMR overall dataframe sourced from RDS in global.R
  if (is.null(demography_group_selected)) {
    return(MMR)
  }

  # If user wants to see data from a specific age group, return MMR_AGE dataframe frame sourced from RDS in global.R
  # filtered to that age group
  mmr_df <- if (demography_selected == "age" & demography_group_selected != "all_ages") {
    MMR_AGE %>%
      filter(age_of_mother == demography_group_selected)
    # If user has mmratio selected and wants to see a specific ethnicity group return MMRATIO_ETHNICITY data frame
    # sourced from RDS in global.R filtered to the selected ethnicity group
  } else if (mmratio & demography_selected == "ethnicity" & demography_group_selected != "all_ethnicities") {
    MMRATIO_ETHNICITY %>%
      filter(ethnicity_race == demography_group_selected)
    # If age is selected and age group is all ages, or if ethnicity is selected and ethnicity group is all ages
    # return MMR overall dataframe sourced from RDS in global.R
  } else {
    MMR
  }

  # Filter out NJ summaries that are only used in rel_mmr_df2() (Explore More tab)
  mmr_df <- mmr_df %>%
    filter(County_Name != "All Counties of NJ")
}

#' Function to determine maternal mortality data frame (sourced from global.R) should be used based on
#' the user's input values - options: MMR_AGE and MMRATIO_ETHNICITY. If the data frame has both maternal
#' mortality measurements within it, the function removes the unused measurement. Capitalized variables
#' MMRATIO_PER_100KLB is sourced from global.R
#'
#' @param mm_selected - the user's selected measurement (derived from input$mm_select_box2, options:
#' 'mmrate_per_1kyl' or 'mmratio_per_100klb')
#' @param year_selected - the user's selected year (derived from input$mm_slide_year2, options: 2005 to 2017)
#' @param demography_selected - the user's selected demography (derived from input$demography_radio, options:
#' 'age' or 'ethnicity')
#' @param county_selected - the user's selected NJ county (derived from input$mm_county_box)
#'
#' @return A dataframe pertaining to the user's selected crtiera
mmr_df_explore_more <- function(mm_selected, year_selected, demography_selected, county_selected) {

  # MMR_AGE and MMRATIO_ETHNICITY from global.R
  df <- if (demography_selected == "age") {
    MMR_AGE
  } else if (demography_selected == "ethnicity" & req(mm_selected == MMRATIO_PER_100KLB)) {
    MMRATIO_ETHNICITY
  }

  # MMRATIO_PER_100KLB from global.R
  other_measure <- ifelse(mm_selected == MMRATIO_PER_100KLB, "mmrate", "mmratio")

  df <- df %>%
    filter(year == year_selected, County_Name == county_selected) %>%
    select(-starts_with(other_measure), -FIPS_Code) %>%
    na.omit()

  # If the sum of our rate/ratio is zero, return 0 for empty graph
  if (df %>% pull(mm_selected) %>% sum(na.rm = TRUE) == 0) 0 else df
}
