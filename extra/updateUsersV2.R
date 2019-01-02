#
# 03-09-2018
#
# Script which checks for updates of user against a file in checkinHist
# This should be run from a bat file with a timer.
#
.libPaths("c:/R_libs")

path = "C:/Users/jqmt/Desktop/jens/drinkioso/"
setwd(path)
#
rm(list = ls())
# functions
source("extra/getUsers.R")
source("userHistoryV3.R")
#
users = getUsers()
#
for (i in users){
  print(i)
  getUserHist(user = i, wTime = 10)
}
