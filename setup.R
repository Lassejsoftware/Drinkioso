#
# 14-09-2018
#
# Loads some stuff and make it global
#
.libPaths(c("/home/shiny/R_libs", .libPaths())) # libs on aws
shinyOptions(cache = diskCache("./myapp-cache"))
# Packages
library(shiny)
library(tidyr)
library(plyr)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(curl)
library(jsonlite)

# Map
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
source("extra/getTeam.R")
source("extra/getVenueCheckIn.R")
source("userHistoryV3.R")

# user stats
source("userStats/userPlotWrapper.R")
source("userStats/userHist.R")
source("userStats/userTimeline.R")
source("userStats/trophyWrapper.R")

# Team stats
source("teamStats/teamPlotWrapper.R")
source("teamStats/teamTimeline.R")
source("teamStats/teamHist.R")

# Venue stats
source("venueStats/venueTimePlot.R")

# Variables
cph <<- readRDS("maps/cph.rds")
