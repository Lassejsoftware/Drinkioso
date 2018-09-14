#
# 29-08-2018
#
# Get user history. Used to get the beer history for new users. Updates location list if needed.
#
# TODO: Make it into a function. Check code == 200( perhaps with a fromJSON wrapper?). Make the segmentation into a function
#       instead of id and secret have config$id etc
#
.libPaths("c:/R_libs")
library(jsonlite)
rm(list = ls())
# functions
source("extra/makeVenueNames.R")
source("extra/config.R") # Secret, id and setup.
source("extra/segmentUserCheckins.R") # This is how we segment incomming data.
#
getFunc <- function(lst,n1,n2 = NULL){
  if (is.null(n2)){
    out = lst[[n1]]
    if (is.null(out)){
      out = NA
    }
  } else {
    out = lst[[n1]][[n2]]
    if (is.null(out)){
      out = NA
    }
  }
  return(out)
}
# API setup
method = "user/checkins/"
lim = 50 # Untappd does not support more than 50 beer returns a time
wTime = 2*60 # waiting time between api call + calc
#
user = "Garbacz"
path = "C:/Users/jqmt/Desktop/jens/untappd/"
setwd(path)
# Get relevant data
S = 1 # switch for the end of user history is reached.
max_id = NULL
while ( S == 1){
  if (is.null(max_id)){ # First api call
    print("First call")
    final = paste0(link,method,user,"?client_id=",id,"&client_secret=",secret,"&limit=",lim)
    userCheckins = fromJSON(final)
    #items = userCheckins$response$checkins$items # Get the data
  } else {
    print("Now with max_id")
    final = paste0(link,method,user,"?client_id=",id,"&client_secret=",secret,"&limit=",lim,"&max_id=",max_id)
    userCheckins = fromJSON(final)
    #items = userCheckins$response$checkins$items # Get the data
  }
  # dfTot = segmentUserCheckins(items)
  dfTot = data.frame(checkin_id = items$checkin_id) # make data frame
  for (i in names(vars)){
    temp = items[[i]]
    if (length(vars[[i]]) != 0 ){ 
      temp = temp[,vars[[i]]]
    } 
    # Well i suppose we could do some merge magic instead?
    namesTot = names(dfTot)
    namesTemp = names(temp)
    if ("items" %in% namesTemp){
      print(paste0(i," does not seem to be simple"))
      a=b
    }
    if (length(namesTemp) == 0){
      if (!(i %in% namesTot)){
        dfTemp = data.frame(temp)
        names(dfTemp) <- i
        dfTot = cbind(dfTot,dfTemp)
      }
    } else {
      print(paste0("Throwing away ", intersect(namesTemp,namesTot)))
      temp = temp[,setdiff(namesTemp,namesTot)]
      dfTot = cbind(dfTot,temp)
    }
  }
  # Venue is a bit special
  temp = items$venue
  for (i in venVars){
    temp2 = unlist(lapply(temp, getFunc,n1=i))
    dfTemp = data.frame(temp2)
    names(dfTemp) <- i
    dfTot = cbind(dfTot,dfTemp)
  }
  
  if (dim(items)[1]<lim){
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
 
# Check if we have the users info
users = dir("users/")
if (!(user %in% users)){
  method = "user/info/"
  final = paste0(link,method,user,"?client_id=",id,"&client_secret=",secret)
  userInfo = fromJSON(final)
  listUser = userInfo$response$user
  saveRDS(listUser, file = paste0("users/",user,".rds"))
  print("Wating for next call")
  Sys.sleep(wTime)
}

# Check if the user has been at some non indexed venues.
venueNames = makeVenueNames(dfLarge)
unVenue = unique(venueNames)
venues = dir("venues/")
inds = which(!(unVenue %in% venues))
unVenue = unVenue[inds]
dfVenue = makeVenueNames(unVenue, inv = 1)
method = "venue/info/"
for (i in 1:dim(dfVenue)[1]){
  final = paste0(link,method, dfVenue$venue_id[i], "?client_id=",id,"&client_secret=",secret)
  venueInfo = fromJSON(final)
  listVenue = venueInfo$response$venue
  saveRDS(listVenue, file = paste0("venues/", unVenue[i], ".rds"))
  print("Wating for next call")
  Sys.sleep(wTime)
}
#
# Testing
#
final = paste0(link,method,user,"?client_id=",id,"&client_secret=",secret,"&limit=50")
userCheckins = fromJSON(final)
u1 = userCheckins
#
max_id = userCheckins$response$pagination$max_id
final = paste0(link,method,user,"?client_id=",id,"&client_secret=",secret,"&limit=50","&max_id=",max_id)
u2 = fromJSON(final)
