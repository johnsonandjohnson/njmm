library(assertthat)
library(classInt)
library(leaflet)

#' Function to create custom palette for measures of maternal mortality
#'
#' @param mmr_values: numeric vector of maternal mortality measures (comes rel_mmr_df())
#' @param mm_selected: string indicating the measure of maternal mortality selected (comes from input$mm_selected)
#'
#' @return a color palette
create_custom_pal <- function(mmr_values, mm_selected) {
  # Switch theoretical max based on measure selected
  theoretical_max <- switch(mm_selected,
    "mmratio_per_100klb" = 100000,
    "mmrate_per_1kyl" = 1000
  )

  # Split values into those above/below theoretical max
  valid_mmr_values <- split(mmr_values, mmr_values >= theoretical_max)

  # Create fisher-jenks breaks
  # Note - the number of breaks chosen was 7 because 9 is the max number allowed for Red color,
  # when a custom break is added this will maximize the number of colors allowed
  intervals <- classIntervals(valid_mmr_values[[1]],
    n = 7, style = "fisher", warnSmallN = FALSE
  )
  breaks <- intervals$brks


  # If values are above the theoretical max, add them as a custom extreme break
  if (length(valid_mmr_values) > 1) {
    extreme_brks <- list(theoretical_max, max(mmr_values, na.rm = TRUE)) %>%
      as.numeric()
    breaks <- breaks %>% append(extreme_brks)
  }

  # Return color palette (global variables come from global.R)
  colorBin(
    palette = ifelse(mm_selected == MMRATIO_PER_100KLB,
      COLORS_MMRATIO, COLORS_MMRATE
    ),
    bins = unique(breaks)
  )
}
