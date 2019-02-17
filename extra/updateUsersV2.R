#
# 03-09-2018
#
# Script which checks for updates of user against a file in checkinHist
# This should be run from a bat file/ cron job with a timer.
# Todo: fix path. Perhaps make it a server environment variable? 
# 
#
path = "/srv/shiny-server/drinkioso/"
setwd(path)

logName =  paste0("../drinkiosoData/logs/",gsub("\\.", "_",make.names(Sys.time())),".txt")
print(logName)

zz <- file(logName, open = "wt")
sink(zz)
sink(zz, type = "message")

.libPaths(c("/home/shiny/R_libs","c:/R_libs", .libPaths())) # libs on aws
#
library(jsonlite)
library(plyr)
#
# rm(list = ls())
# functions
source("extra/getUsers.R")
source("userHistoryV3.R")
source("userStats/trophyWrapper.R")
source("userStats/calcWarrior.R")
source("userStats/calcColumbus.R")
source("extra/getTeam.R")
source("helperFunctions.R")
#
users = getUsers(team = T)
#
for (i in users){
  print(i)
  getUserHist(user = i, wTime = 60)
}

# Delete plot cache
file.remove(dir("myapp-cache/",full.names = T))