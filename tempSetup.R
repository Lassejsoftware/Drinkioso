#
# 03-09-2018
#
# Temp setup file.
.libPaths("c:/R_libs")
rm(list = ls())
path = "C:/Users/jqmt/Desktop/jens/untappd/"
setwd(path)
library(jsonlite)
# Helper functions
source("extra/makeVenueNames.R")
source("extra/unpackCheckins.R")
source("extra/untappdAPI.R")
source("extra/config.R")
source("extra/checkForNewVenues.R")
source("userHistoryV2.R")
#
# getUserHist. Test user = "Garbacz"
users = c("Slendrick","Garbacz", "hellegskov")
getUserHist(user = "Garbacz", maxSiz = 200, wTime = 30) # Seems to work
# user info
method = "user/info/"
test = untappdAPI(method = method, param = "knoe1703")
# Some small user hist. maxSiz = 350 seems to be the limit.
for (i in users){
  getUserHist(user = i, maxSiz = 500, wTime = 5)
}
getUserHist(user = "knoe1703", maxSiz = 350, wTime = 5)
