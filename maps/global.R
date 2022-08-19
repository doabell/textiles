# constants
# https://shiny.rstudio.com/articles/scoping.html


# @Middlebury College - 2021
# Interactive textile explorer
# Originally authored by Ev Berger-Wolf, Camryn Kluetmeier, Jason Rickenbacker, and Nicholas Sliter
# Under the instruction of Prof. Carrie Anderson at Middlebury College
# Code maintained and extended by Nicholas Sliter
# Imported from commit 66a578f
# https://github.com/Nicholas-Sliter/textile_map/commit/66a578f89572385c421833fcc2cd7f0f71260795

# libraries
library(shiny)
library(readxl)
library(showtext)
library(tidyverse)
library(stringr)
library(leaflet)
library(viridis)
# library(plotly)
library(shinyWidgets)
library(shinythemes)

# source to function file
source("functions.R")

# Font for ggplot
font_add_google("Lato", "Lato")
showtext_auto()

CONSTANTS <- c(
  "SHINY_THEME" = "sandstone",
  "COLOR_THEME" = "magma",
  "COLORS" = toString(c("white", "yellow", "red", "blue", "purple", "green", "black", "brown", "grey", "silver", "gold"))
)

# make copies of original data
joined.data <- readRDS("week4.rds")

# convert JSON col to nonJSON
# joined.data <- joined.data.original %>% mutate(colorList = vec_unflatten(colorList))


# Fix Facet Wrapping Issue (deal with this after presentation)
# joined.data$textile_quality_inferred <- factor(joined.data$textile_quality_inferred,
#                                              levels = c("Inexpensive", "Mid-Range", "Expensive"))
map.data <- readRDS("hist_geo.rds")

# modVec

# Creating a modifier choice vector
modVec <- c(
  "Textile Name" = "textile_name",
  # "Color" = "colorGroup",
  "Color" = "textile_color_arch",
  "Pattern" = "textile_pattern_arch",
  "Process" = "textile_process_arch",
  "Fiber Type" = "textile_fiber_arch",
  "Geography" = "textile_geography_arch",
  "Quality" = "textile_quality_arch"
)

# Limit 2e+05 notation
# There is a better way with 
# require(scales)
# ggplot() + scale_x_continuous(labels = comma)
# But being lazy for now
# https://stackoverflow.com/questions/14563989/

# For digits, see ?format
options(scipen=5, digits = 2)
