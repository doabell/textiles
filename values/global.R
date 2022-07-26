# Libraries ####

library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(readr)


# Read data ####

wicvoc <- readRDS("week4.rds")

# make year discrete
wicvoc$orig_yr <- factor(wicvoc$orig_yr)
