#
# 03-09-2018
#
# Script which checks for updates of user against a file in checkinHist
# This should be run from a bat file/ cron job with a timer.
# Todo: fix path. Perhaps make it a server environment variable? 
#
.libPaths(c("/home/shiny/R_libs","c:/R_libs", .libPaths())) # libs on aws
#
if ("bleh"){
  path = "C:/Users/jqmt/Desktop/jens/drinkioso/"
}
setwd(path)
startDate = readRDS("extra/data/startDate.rds")
cph <- readRDS("maps/cph.rds")
#
rm(list = ls())
# functions
source("extra/getUsers.R")
source("userHistoryV3.R")
source("userStats/trophyWrapper.R")
source("userStats/calcWarrior.R")
source("userStats/calcColumbus.R")
#
users = getUsers(team = T)
users = subset(users, !(users %in% noUsers))
#
for (i in users){
  print(i)
  getUserHist(user = i, wTime = 60)
}


# Recalc user stats
trophies = trophyWrapper(opts = "trophies")
for (i in trophies){
  trophyWrapper(trophy = i, startDate = startDate, map = , reCalc = T)
}