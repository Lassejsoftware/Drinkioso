#
# 29-08-2018
#
# Get user history. Used to get the beer history for new users. Updates location and user folder if needed.
#
# TODO: Make it into a function. 
#
.libPaths("c:/R_libs")
path = "C:/Users/jqmt/Desktop/jens/untappd/"
setwd(path)
#
library(jsonlite)
rm(list = ls())
# functions
source("extra/makeVenueNames.R")
# source("extra/config.R") # Secret, id and setup.
source("extra/unpackCheckins.R") 
source("extra/untappdAPI.R")
#
# API setup
method = "user/checkins/"
lim = 50 # Untappd does not support more than 50 beer returns a time
wTime = 2*60 # waiting time between api call + calc
#
user = "Garbacz"
# Get relevant data
S = 1 # switch for the end of user history is reached.
max_id = NULL
while ( S == 1){
  if (is.null(max_id)){ # First api call
    print("First call")
    query = list(limit = 50)
    userCheckins = untappdAPI(method = method, param = user, query = query)
  } else {
    print("Now with max_id")
    query = list(limit = lim,
                 max_id = max_id)
    userCheckins = untappdAPI(method = method, param = user, query = query)
  }
  dfTot = unpackCheckins(userCheckins)
  if (dim(dfTot)[1]<lim){
    print("We reached the end or lim was set to high")
    S = 0
  }
  if (is.null(max_id)){
    dfLarge = dfTot
  } else {
    dfLarge = rbind(dfLarge,dfTot)
  }
  max_id = userCheckins$response$pagination$max_id # update max_id
  rm(dfTot)
  print("Waiting for next call")
  Sys.sleep(wTime)
}
saveRDS(dfLarge, file = paste0("data/",user,".rds"))
 
# Check if we have the users info on file
users = dir("users/")
if (!(user %in% users)){
  method = "user/info/"
  userInfo = untappdAPI(method = method, param = user)
  listUser = userInfo$response$user
  saveRDS(listUser, file = paste0("users/",user,".rds"))
  print("Wating for next call")
  Sys.sleep(wTime)
}

# Check if the user has been at some non indexed venues.
dfVenue = makeVenueNames(dfLarge)
dfVenue = unique(dfVenue)
venues = dir("venues/")
inds = which(!(dfVenue$name %in% venues))
dfVenue = dfVenue[inds,]
method = "venue/info/"
for (i in 1:dim(dfVenue)[1]){
  venueInfo = untappdAPI(method = method, param = dfVenue$venue_id[i])
  listVenue = venueInfo$response$venue
  saveRDS(listVenue, file = paste0("venues/", dfVenue$name[i]))
  print("Wating for next call")
  Sys.sleep(wTime)
}

