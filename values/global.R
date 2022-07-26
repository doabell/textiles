##Importing Libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)

wicvoc <- readRDS("week4.rds")
# wicvoc <- read_csv("final_data.csv")
wicvoc$orig_yr <- factor(wicvoc$orig_yr)
#This line makes year a discrete variable