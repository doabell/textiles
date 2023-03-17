# Libraries ####

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(stringr)
library(magrittr)
library(plotly)
library(readr)


# Read data ####
wicvoc <- readRDS("week4.rds")
unitvec <- readRDS("unitvec.rds")

# Modifier columns
modvec <- c(
  "textile_color_arch",
  "textile_color_inf",
  "textile_pattern_arch",
  "textile_process_arch",
  "textile_fiber_arch",
  "textile_geography_arch",
  "textile_quality_arch",
  "textile_other_unknown_arch"
)

# Modifier column names
modnames <- c(
  "Color, archival",
  "Color, inferred",
  "Pattern, archival",
  "Process, archival",
  "Fiber, archival",
  "Geography, archival",
  "Quality, archival",
  "Other or unknown, archival"
)