#
# 03-09-2018
#
# Script which checks for updates of user against a file in checkinHist
# This should be run from a bat file with a timer.
#
.libPaths("c:/R_libs")
path = "C:/Users/jqmt/Desktop/jens/untappd/"
setwd(path)
library(jsonlite)
library(plyr)
#
rm(list = ls())
# functions
source("extra/config.R")
source("extra/unpackCheckins.R")
source("extra/untappdAPI.R")
source("extra/checkForNewVenues.R")
source("extra/makeVenueNames.R")
#
# Rip from plyr without warnings.
row_match <- function (x, y, on = NULL) 
{
  if (is.null(on)) {
    on <- intersect(names(x), names(y))
  }
  keys <- join.keys(x, y, on)
  x[keys$x %in% keys$y, , drop = FALSE]
}
# API setup
method = "user/checkins/"
wTime = 5#2*60 # Wait time between api calls and calculations. Only 100 calls per hour is allowed.
#
users = dir("checkinHist/")
users = gsub(pattern = "\\.rds", replacement = "", x = users)
#
for (i in users){
  print(i)
  cHist = readRDS(paste0("checkinHist/",i,".rds"))
  print("Getting checkins")
  newCheckinsReturn = untappdAPI(method = method, param = i, query = list(limit = 50))
  print("Done")
  newCheckins = unpackCheckins(newCheckinsReturn)
  #
  temp = row_match(cHist, newCheckins )
  if (dim(temp)[1] != dim(newCheckins)[1]){
    if (dim(temp)[1] == 0){
      dfLarge = newCheckins
      S = 1
      while (S == 1){
        # Woah more than 50 new beers!
        Sys.sleep(wTime)
        max_id = newCheckinsReturn$response$pagination$max_id
        olderChecksRet = untappdAPI(method = method, param = i, query = list(limit = 50, max_id = max_id))
        olderChecks = unpackCheckins(olderChecksRet)
        temp = row_match(cHist, olderChecks)
        dfLarge = rbind(dfLarge,olderChecks)
        if (dim(temp)[1] != 0){
          cHist = rbind(cHist, dfLarge)
          cHist = unique(cHist)
          S = 0
        }
      }
    } else {
      # New Checkins!
      cHist = rbind(cHist,newCheckins)
      cHist = unique(cHist) # This is sloppy.
    }
    cHist = cHist[order(cHist$checkin_id, decreasing = T),]
    saveRDS(cHist, file = paste0("checkinHist/",i,".rds") )
  }
  #
  print("getting new venues")
  checkForNewVenues(cHist)
}
