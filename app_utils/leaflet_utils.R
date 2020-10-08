pacman::p_load(dplyr, leaflet, leaflet.extras, stringr)

#' Function to create leaflet NJ Map with consistent default seetings for both maps
#'
#' @return: a leaflet map of NJ
basic_map_NJ <- function() {
  leaflet(options = leafletOptions(
    zoomControl = TRUE, dragging = TRUE,
    minZoom = 7.8, maxZoom = 9
  )) %>%
    addMapPane("polygons", zIndex = 200) %>%
    enableTileCaching() %>% # Add tile caching
    # Base map & caching options
    addProviderTiles(providers$CartoDB.Positron,
      options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
    ) %>%
    setView(
      lng = -74.406,
      lat = 40.058,
      zoom = 7.8
    ) # Set view to NJ
}

#' Function to create consistent blue highlighting for both maps
#'
#' @return: blue highlight options for map
map_highlight <- function() {
  highlightOptions(
    bringToFront = FALSE,
    opacity = 1,
    weight = 5,
    sendToBack = FALSE,
    color = "blue"
  )
}

#' Function to create consistent label formatting for both maps
#'
#' @return: label options for map
map_label <- function() {
  labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "10px",
    opacity = .9,
    direction = "auto"
  )
}

#' Function to add the delivery hospitals layer to either map
#' @param base_map: a leaflet map object
#'
#' @return: delivery hospital layer with checkbox
hospital_layer <- function(base_map) {
  base_map %>%
    addCircleMarkers(
      data = BIRTH_HOSPS, # Global variable imported in global.R
      lng = ~LONGITUDE, lat = ~LATITUDE,
      label = sprintf("<strong>%s</strong>", BIRTH_HOSPS$NAME) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto"
      ),
      radius = 3, group = "Delivery Hospitals"
    ) %>%
    addLayersControl(
      overlayGroups = "Delivery Hospitals",
      options = layersControlOptions(collapsed = FALSE, position = "bottomright")
    ) %>%
    hideGroup("Delivery Hospitals")
}

#' Function that takes in te contextual variable leaflet map and returns it with the chosen polygons layer
#'
#' @param leaflet_map: the map to add polygons onto
#' @param color_scheme: the color scheme of the polygons
#' @param layer_name: the name shown on the labels and radio button
#' @param max_value: the max value of the data
#' @param data_values: the data to map
#'
#' @return: leaflet map with added polygons
add_polygons_layer_map <- function(leaflet_map, color_scheme, layer_name, max_value, data_values) {
  data_labels_ending <- case_when(
    grepl("Percent", layer_name) ~ paste("%", str_replace(layer_name, "Percent ", "")),
    grepl("NTSV", layer_name) ~ "% Difference in NTSV Cesarean Rate",
    grepl("Per Capita", layer_name) ~ paste0(" ", str_replace(layer_name, "Per Capita", "Per 100K"))
  )

  # This layer will have negative values and use a reversed palette
  min_value <- if ("Difference in NTSV Rate" == layer_name) -(max_value) else 0
  pal_reverse <- if ("Difference in NTSV Rate" == layer_name) TRUE else FALSE

  leaflet_map %>%
    addPolygons(
      data = COUNTIES, # Global variable imported in global.R
      color = "black",
      fillColor = ~ colorNumeric(color_scheme, c(min_value, max_value), reverse = pal_reverse)(data_values),
      fillOpacity = .7,
      weight = 2,
      group = layer_name,
      highlight = map_highlight(),
      label = paste0(data_values, data_labels_ending),
      options = pathOptions(pane = "polygons")
    )
}

#' Function that takes in the slider year and returns the year to use for OBGYN data
#'
#' @param year_selected: the selected year of the slider
#'
#' @return: the obgyn year of data shown
obgyn_year <- function(year_selected) {
  obgyn_year <- case_when(
    between(year_selected, 2010, 2014) ~ 2010,
    between(year_selected, 2015, 2016) ~ 2015,
    year_selected == 2017 ~ 2017
  )
  return(obgyn_year)
}