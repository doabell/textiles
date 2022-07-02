# constants
# https://shiny.rstudio.com/articles/scoping.html

# libraries
library(shiny)
library(readxl)
library(rgdal)
library(tidyverse)
library(stringr)
library(debkeepr)
library(leaflet)
library(viridis)
library(jsonlite)
library(plotly)
library(shinyWidgets)
library(shinythemes)

#source to function file
source('functions.R')

CONSTANTS <- c(
  
  
  "SHINY_THEME" = "sandstone",
  "COLOR_THEME" = "magma",
  "COLORS" = toString(c("white","yellow","red","blue","purple","green","black", "brown", "grey", "silver", "gold"))
  
  
  
)

#Read in the data
joined.data.original <- read_csv("joined.csv")
map.data.original <- readOGR("filteredCountries.GeoJSON")

#make copies of original data
joined.data <- joined.data.original

#convert JSON col to nonJSON
#joined.data <- joined.data.original %>% mutate(colorList = vec_unflatten(colorList))


#Fix Facet Wrapping Issue (deal with this after presentation)
#joined.data$textile_quality_inferred <- factor(joined.data$textile_quality_inferred,
 #                                              levels = c("Inexpensive", "Mid-Range", "Expensive"))
map.data <- map.data.original

# Create zoom locations
latLongZoom.original <- data.frame("Area" = c("World", "Europe", "Africa",
                                              "Middle East", "Pacfic Islands", "Asia"),
                                   "Lat" = c(30, 49.8, -6, 27, 0, 32),
                                   "Long" = c(53, 15.47, 30, 72.5, 116, 115),
                                   "Magnify" = c(2, 4.25, 2.5, 4, 4, 3.25))

latLongZoom <- latLongZoom.original

# modVec

#Creating a modifier choice vector
modVec <- c("Textile Name" = "textile_name",
            #"Color" = "colorGroup",
            "Color" = "colorList",
            "Pattern" = "textile_pattern_arch",
            "Process" = "textile_process_arch",
            "Fiber Type" = "textile_fiber_arch",
            "Value Range" = "textile_quality_inferred",
            "Geography" = "textile_geography_arch",
            "Quality" = "textile_quality_arch")
