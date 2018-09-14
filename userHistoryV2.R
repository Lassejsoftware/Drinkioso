#
# 03-09-2018
# userHistoryV2. Now a function mother fucker!
#
# Todo: Make some sort of progress indicator. Perhaps branch out checkUser. 
#
# dep: source("extra/makeVenueNames.R"); source("extra/unpackCheckins.R"); source("extra/untappdAPI.R"); source("extra/checkForNewVenues.R")
#
getUserHist <- function(user = NULL, maxSiz = 20000, wTime = 2*60, overWrite = 0){
  if (is.null(user)){print("Specify a user, dummy!");return(NULL)}
  # Setup api.
  method = "user/checkins/"
  lim = 50 # Untappd does not support more than 50 beer returns a time
  #
  # Get that user checkin history data
  S = 1 # switch for the end of user history is reached.
  max_id = NULL
  fileName = paste0("checkinHist/",user,".rds")
  if (file.exists(fileName) && overWrite == 0){
    print("User already exists")
    print("If you must update use overWrite = 1")
    return()
  }
  # 
  while ( S == 1){
    if (is.null(max_id)){ # First api call
      print("First call")
      query = list(limit = 50)
      userCheckins = untappdAPI(method = method, param = user, query = query)
    } else {
      print("Now with max_id")
      print(max_id)
      query = list(limit = lim,
                   max_id = max_id)
      userCheckins = untappdAPI(method = method, param = user, query = query)
    }
    dfTot = unpackCheckins(userCheckins)
    if (is.null(max_id)){
      dfLarge = dfTot
    } else {
      dfLarge = rbind(dfLarge,dfTot)
    }
    if (dim(dfTot)[1]<lim || dim(dfLarge)[1]>=maxSiz){
      print("We reached the end or limit was set to high")
      S = 0
    }
    print(dim(dfLarge)[1])
    max_id = userCheckins$response$pagination$max_id # update max_id
    rm(dfTot)
    saveRDS(dfLarge, file = fileName) # 
    print("Waiting for next call")
    Sys.sleep(wTime)
  }
  
  # Check if we have the users info on file
  users = dir("users/")
  if (!(paste0(user,".rds") %in% users)){
    method = "user/info/"
    userInfo = untappdAPI(method = method, param = user)
    listUser = userInfo$response$user
    saveRDS(listUser, file = paste0("users/",user,".rds"))
    print("Waiting for next call")
    Sys.sleep(wTime)
  }
  
  # Check if the user has been at some non indexed venues.
  checkForNewVenues(dfLarge)
}
