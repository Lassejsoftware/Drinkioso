#
# 14-09-2018
#
# Loads some stuff and make it global
#
# Packages
library(tidyr)
library(dplyr)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)

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
# source("userHistoryV3.R")

# user stats
source("userStats/userPlotWrapper.R")
source("userStats/userHist.R")
source("userStats/userTimeline.R")
source("userStats/trophyWrapper.R")

# Team stats
source("teamStats/teamPlotWrapper.R")
source("teamStats/teamTimeline.R")
source("teamStats/teamHist.R")

# Variables
cph <<- readRDS("maps/cph.rds")
