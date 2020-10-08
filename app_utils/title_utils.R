#' Function to build the mmratio or mmrate map title. Capitalized variable MMRATIO_PER_100KLB is
#' sourced from global.R
#'
#' @param year_selected - the user's selected year derived from input$mm_slide_year
#' @param mm_selected - the user's selected measurement derived from input$mm_select_box
#' @param demography_selected - the user's selected demography derived from input$demography_select_box
#' @param demography_group_selected - the specific age or ethnicity group pertaining
#' to the demography_selected, derived from input$age_select_box or input$demography_select_box
#'
#' @return An HTML title specific to the selected maternal mortality measurement, year, and demographic
build_map_title <- function(year_selected, mm_selected = c("mmratio_per_100klb", "mmrate_per_1kyl"), demography_selected, demography_group_selected) {
  rate_or_ratio <- if (mm_selected == MMRATIO_PER_100KLB) "Ratio" else "Rate"

  title <- paste(year_selected, "Maternal Mortality", rate_or_ratio, paste0("by County"))

  # If there is no specific demography selected, return base title
  if (demography_selected == "all") {
    return(title %>% HTML())
  }

  # Build title ending specific to demography group selected
  title_ending <- case_when(
    demography_selected == "age" & demography_group_selected == "all_ages" ~ "All Ages",
    demography_selected == "ethnicity" & demography_group_selected == "all_ethnicities" ~ "All Ethnicities",
    demography_selected == "age" & demography_group_selected != "all_ages" ~ paste("Ages", demography_group_selected),
    demography_selected == "ethnicity" & demography_group_selected != "all_ethnicities" ~ paste0(demography_group_selected, "</br>", "Ethnicity")
  )

  title <- paste(paste0(title, "</br>", "for Women of"), title_ending, collapse = " ") %>%
    HTML()

  return(title)
}

#' Function to build the layer map title
#'
#' @param year_selected - the user's selected year derived from input$mm_slide_year
#' @param layer_selected - the user's selected layer derived from input$other_layer_box
#'
#' @return An HTML title specific to the selected year and layer
build_layer_map_title <- function(year_selected, layer_selected) {
  if (year_selected >= 2010) {

    # Change title of NTSV Cesarean Rate
    layer_selected <- if (layer_selected == "Difference in NTSV Rate") "Difference in NTSV Cesarean Rate</br>from 2020 NJ Statewide Goal" else layer_selected

    # Change year to reflect the data being shown
    year <- if (layer_selected == "OB/GYN Physicians Per Capita") obgyn_year(year_selected) else year_selected

    title <- layer_selected %>%
      paste(year, ., "by County")

    # Add age groups on the end if it's an insurance layer
    if (layer_selected == "Percent Covered by Medicaid" || layer_selected == "Percent Uninsured") {
      title <- title %>%
        paste0("</br>", "for Females Ages 6-54")
    }

    if (year_selected != year) {
      title <- title %>%
        paste(
          "<br/>", icon("exclamation-circle"),
          "<i>Years differ due to data availability.</i>"
        )
    }
  } else {
    title <- "Other Layer data is only available</br>from 2010 forwards."
  }

  return(title %>% HTML())
}

#' Function to build the graph title. Capitalized variable MMRATIO_PER_100KLB is
#' sourced from global.R
#'
#' @param empty_graph - boolean describing if there is data for the user's selected inputs
#' @param year_selected - the user's selected year
#' @param mm_selected - the user's selected measurement
#' @param county_selected - the user's selected country
#'
#'
#' @return An HTML title specific to the selected maternal mortality measurement, year, county
build_graph_title <- function(empty_graph, year_selected, mm_selected = c("mmratio_per_100klb", "mmrate_per_1kyl"), county_selected) {
  rate_or_ratio <- if (mm_selected == MMRATIO_PER_100KLB) "Ratio" else "Rate"

  if (empty_graph) {
    title <- paste("Maternal Mortality", paste0(rate_or_ratio, "s"), "in", county_selected, "County were all 0 for", year_selected)
  } else {
    title <- paste(year_selected, "Maternal Mortality", paste0(rate_or_ratio, "s"), "for", county_selected)
    if (county_selected != "All Counties of NJ") {
      title <- paste(title, "County")
    }
  }

  return(title %>% HTML())
}
