library(magrittr)

#' Function used to build the map labels shown when hovering over a county.
#'
#' @param mmr_merge - the dataframe to use
#' @param mm_selected - the user's selected maternal mortality measurement (comes from input$mm_select_box)
#'
#' @return An html label showing the number of maternal deaths, mm measurement selected, and the number
#' of live births or woman years lived (dependent on the mm measurement selected)
build_map_label <- function(mmr_merge, mm_selected) {
  # Denominator string based on selector
  denom_string <- ifelse(mm_selected == "mmratio_per_100klb",
    "MMRatio",
    "MMRate"
  )

  denom_desc <- ifelse(mm_selected == "mmratio_per_100klb",
    "live births",
    "years lived"
  )

  # Actual label html
  sprintf(
    paste(
      "<strong>%s</strong><br/>",
      "%s maternal deaths<br/>",
      "%s", denom_desc, "<br/>",
      "%s", denom_string
    ),
    mmr_merge$COUNTY,
    mmr_merge$maternal_deaths %>%
      prettyNum(big.mark = ","),
    mmr_merge %>%
      pull(gsub(" ", "_", denom_desc)) %>%
      prettyNum(big.mark = ","),
    mmr_merge %>%
      pull(mm_selected) %>%
      prettyNum(big.mark = ",", scientific = FALSE)
  ) %>%
    lapply(HTML)
}
