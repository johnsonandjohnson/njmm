# library(mapview)
library(rgdal)
# library(mapedit)
library(magrittr)

counties <- readOGR("data/New_Jersey_Counties/New_Jersey_Counties.shp")
highways <- readOGR("data/New_Jersey_Highways/tl_2015_34_prisecroads.shp")

# View by attribute with zcol()
mapview(counties, zcol = c("COUNTY", "SQ_MILES"), legend = T)

# Add new attribute MMRate and view with new attribute
counties$MMRate <- c(5, 6, 3, 4, 8, 7, 10, 3, 5, 4, 7, 6, 3, 5, 4, 2, 7, 2, 5, 3, 5)
mapview(counties, zcol = c("COUNTY", "MMRate"), legend = T)

# Adjust color scheme to show severity of MMRates by county
mapviewOptions(vector.palette = colorRampPalette(c("green", "gold", "red")))
mapview(counties, zcol = c("MMRate"), legend = T)

# Adjust opacity and add highways layer
mapview(counties, zcol = c("MMRate"), legend = T, alpha.regions = .5) + mapview(highways, lwd = 1)

# View extents
viewExtent(counties)
mapviewOptions(default = T)
viewExtent(counties) + mapview(counties, zcol = "SQ_MILES")

# Interactively draw on blank map using editMap()
edited_nj <- mapview(counties, legend = F) %>% editMap()
mapview(edited_nj$finished)

# Use breweries to show points
mapview(breweries)

# Can show magnitudes
mapview(breweries, cex = "number.of.types")

# Can layer
mapview(franconia, zcol = "district") + mapview(breweries)
