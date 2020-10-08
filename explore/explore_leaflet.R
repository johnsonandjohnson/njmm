library(leaflet)
library(rgdal)

counties <- readOGR("data/New_Jersey_Counties/New_Jersey_Counties.shp")
highways <- readOGR("data/New_Jersey_Highways/tl_2015_34_prisecroads.shp")

# For some reason counties needed transforming before plotting
counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Faux MMRatios
counties$MMRatio <- c(5, 6, 3, 4, 8, 7, 10, 3, 5, 4, 7, 6, 3, 5, 4, 2, 7, 2, 5, 3, 5)

# Heat color palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = counties$MMRatio
)
# Text upon hovering on counties
labels <- sprintf(
  "<strong>%s</strong><br/>%g maternal deaths / 100,000 live births",
  counties$COUNTY, counties$MMRatio
) %>% lapply(htmltools::HTML)

# Map call
map <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # Base map
  setView(
    lng = -74.406,
    lat = 40.058,
    zoom = 8
  ) %>% # Set view to NJ
  addPolygons(
    data = counties,
    color = ~ pal(counties$MMRatio),
    fillOpacity = 1,
    weight = 1,
    group = "counties", # Group by "counties" in order to call layer toggle
    highlight = highlightOptions(
      bringToFront = FALSE,
      opacity = 1,
      weight = 5,
      sendToBack = FALSE,
      color = "blue"
    ), # Option to highlight upon hover
    label = labels, # Use labels from line 19
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addPolylines(data = highways, color = "black", weight = 1, opacity = 1, group = "highways") %>% # Add highways
  addLayersControl(
    overlayGroups = c("counties", "highways"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% # Add layering on toggle
  addLegend(pal = pal, values = counties$MMRatio, title = "MMRatio", opacity = 1)

map
