# Load Libraries
pacman::p_load(
  ggplot2,
  leaflet,
  microbenchmark,
  rgdal,
  rgeos,
  tictoc
)

# Read in county file
counties <- readOGR("data/raw/nj_counties/New_Jersey_Counties.shp")
counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Faux MMRatios
counties$MMRatio <- c(5, 6, 3, 4, 8, 7, 10, 3, 5, 4, 7, 6, 3, 5, 4, 2, 7, 2, 5, 3, 5)

## Simplify NJ county file
# reference (https://gis.stackexchange.com/questions/236340/simplifying-and-plotting-polygons-in-leaflet-package-in-r)
# Look at various tolerance values
par(mfrow = c(1, 4))
for (i in c(.005, .01, .05, .1)) {
  plot(gSimplify(counties, tol = i),
    main = paste("Simplified -", i)
  )
}

# Write out the file
geo_simple <- gSimplify(counties, tol = .005)
counties_simple <- SpatialPolygonsDataFrame(geo_simple, counties@data)
writeOGR(counties_simple, "data/raw/nj_counties/New_Jersey_Counties_simplified.shp",
  "counties",
  driver = "ESRI Shapefile"
)


## Read in shapefile benchmark
file_read_mb <- microbenchmark(
  orig_file = readOGR("data/raw/nj_counties/New_Jersey_Counties.shp"),
  simp_file = readOGR("data/raw/nj_counties/New_Jersey_Counties_simplified.shp"),
  times = 10
)

autoplot(file_read_mb)


## Leaflet load benchmark
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
make_leaflet <- function(county_file) {
  leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # Base map
    setView(
      lng = -74.406,
      lat = 40.058,
      zoom = 8
    ) %>% # Set view to NJ
    addPolygons(
      data = county_file,
      color = ~ pal(county_file$MMRatio),
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
    )
}

# Leaflet render benchmark
# Need to use tictoc because microbenchmark doesn't play nicely with plotting function
tic("Original County File")
make_leaflet(counties)
toc()

tic("Simplified County File")
make_leaflet(counties_simple)
toc()
