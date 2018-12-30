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

# Map
# library(ggmap)
# library(fields)
# library(spatialfil)
library(leaflet)
library(sp)
library(htmltools)
library(viridis)

# Functions
source("scoring.R")
source("makedfVenue.R")
source("createMapV4.R")
source("mapHoverFunction.R")
source("helperFunctions.R")
source("extra/makeVenueList.R")
source("extra/getUsers.R")
source("userHistoryV3.R")

# user stats
source("userStats/userPlotWrapper.R")
source("userStats/userHist.R")
source("userStats/userTimeline.R")
source("userStats/trophyWrapper.R")


# Variables
cph <<- readRDS("maps/cph.rds")

# Start date
startDate <<- as.Date("01 jan 2018", format = "%d %b %Y")

# as.Date(t3, format = "%d %b %Y")