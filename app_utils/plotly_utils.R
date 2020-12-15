library(assertthat)
library(plotly)

#' Function to plot mmrate or mmratio. To be used with split(.county) and lapply to create a graph
#' for each of 22 counties
#'
#' @param mmrate_or_mmratio_df - a dataframe containing the maternal mortality information to plot
#'
#' @return: A graph with mmrate or mmratio on the y axis and age of mother or race/ethnicity on x axis
one_plot <- function(mmrate_or_mmratio_df) {
  # Change variables based on MMRate vs MMRatio
  if ("mmrate_per_1kyl" %in% colnames(mmrate_or_mmratio_df)) {
    y_axis <- ~mmrate_per_1kyl
    y_label <- "MMRate per 1K Years Lived"
    calc_label <- "Years Lived"
    value <- mmrate_or_mmratio_df$years_lived
    bar_color <- "#41AB5D"
  } else {
    y_axis <- ~mmratio_per_100klb
    y_label <- "MMRatio per 100K Live Births"
    calc_label <- "Live Births"
    value <- mmrate_or_mmratio_df$live_births
    bar_color <- "#EF3B2C"
  }

  # Change variables based on Age of Mother vs Race/Ethnicity
  if ("age_of_mother" %in% colnames(mmrate_or_mmratio_df)) {
    x_axis <- ~age_of_mother
    x_label <- "Age of Mother"
  } else {
    x_axis <- ~ethnicity_race
    x_label <- "Ethnicity/Race"
  }

  plot_ly(
    data = mmrate_or_mmratio_df,
    type = "bar",
    x = x_axis,
    y = y_axis,
    name = " ",
    hovertemplate = paste("Maternal Deaths: ", mmrate_or_mmratio_df$maternal_deaths, "<br>",
      calc_label, ": ", value %>% prettyNum(big.mark = ","), "<br>",
      x_label, ": ", "%{x}",
      sep = ""
    ),
    color = I(bar_color)
  ) %>%
    add_annotations(
      text = ~ unique(County_Name),
      x = .5,
      y = 1,
      xanchor = "center",
      yanchor = "bottom",
      align = "center",
      yref = "paper",
      xref = "paper",
      showarrow = FALSE,
      font = list(size = 15)
    ) %>%
    layout(
      showlegend = FALSE,
      autosize = T,
      xaxis = list(title = x_label),
      yaxis = list(title = y_label)
    ) %>%
    config(modeBarButtonsToRemove = c(
      "select2d", "pan2d", "lasso2d", "zoomIn2d",
      "zoomOut2d", "autoScale2d",
      "toggleSpikelines", "hoverClosestCartesian",
      "hoverCompareCartesian"
    ))
}
