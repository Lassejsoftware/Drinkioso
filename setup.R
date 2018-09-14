#
# 14-09-2018
#
# Loads some stuff and make it global
#
# Packages
library(tidyr)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(ggmap)
library(fields)
library(spatialfil)
library(viridis)

# Functions
source("scoring.R")
source("makedfVenue.R")
source("createMap.R")
# Variables
cph <<- readRDS("maps/cph.rds")
