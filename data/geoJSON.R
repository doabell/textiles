library(tidyverse)
library(geojsonio)
library(leaflet)
library(magrittr)
library(raster)
library(sf)

# get everything under geoJSON/
jsons <- list.files(
  path = "geoJSON",
  pattern = "*",
  recursive = FALSE,
  full.names = TRUE
)

#### read geojson ####
# use leaflet()
# not ggplot-able, needs fortifying
# https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

# cannot_read: missing name data
# Error in h(simpleError(msg, call)) : error in
# evaluating the argument 'x' in selecting a method
# for function 'addAttrToGeom': conversion from
# feature type sfc_GEOMETRY to sp is not supported

# cannot_join: duplicate subscripts for columns

##### with loop #####
for (i in 1:length(jsons)) {
  print(i)
  print(jsons[i])
  geo <- jsons[i] %>%
    as.location() %>%
    geojson_read(what = "sp")
  name <- basename(jsons[i]) %>%
    # remove extension
    tools::file_path_sans_ext()
  geo@data <- geo@data %>%
    mutate(region = name)
  if (i == 1) {
    joined <- geo
    
  } else {
    joined %<>%
      raster::union(geo)
  }
}


# call it a day
joined %>%
  saveRDS("hist_geo.rds")
