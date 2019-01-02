#
# 30-12-2018
# userHistoryV3. 
#
# Todo: Make some sort of progress indicator. 
# Will create or update user checkin history. For new users it also creates a user var in users.
#
getUserHist <- function(user = NULL, wTime = 2*60, overWrite = F){
  if (is.null(user)){print("Specify a user, dummy!");return(NULL)}
  
  library(jsonlite)
  library(plyr)
  
  source("extra/config.R")
  source("extra/unpackCheckins.R")
  source("extra/untappdAPI.R")
  source("extra/checkForNewVenues.R")
  source("extra/makeVenueNames.R")
  
  # Rip from plyr without warnings.
  row_match <- function (x, y, on = NULL) 
  {
    if (is.null(on)) {
      on <- intersect(names(x), names(y))
    }
    keys <- join.keys(x, y, on)
    x[keys$x %in% keys$y, , drop = FALSE]
  }
  
  # Setup api.
  method = "user/checkins/"
  lim = 50 # Untappd does not support more than 50 beer returns a time
  
  # Get the checkins. 
  if (file.exists(paste0("checkinHist/",user,".rds") )){
    cHist = readRDS(paste0("checkinHist/",user,".rds"))
    userHist = T
  } else {userHist = F}
  print("Getting checkins")
  newCheckinsReturn = untappdAPI(method = method, param = user, query = list(limit = lim))
  print("Done")
  newCheckins = unpackCheckins(newCheckinsReturn)
  
  if (!userHist){
    cHist = newCheckins[0,]
    userInfo = untappdAPI(method = "user/info/", param = user)
    userInfo$joinDate = as.POSIXct(Sys.Date(), format = "%Y-%b-%d")
    saveRDS(object = userInfo, paste0("users/",user,".rds"))
  }
  
  # Check if we have them all
  temp = row_match(cHist, newCheckins )
  if (dim(temp)[1] != dim(newCheckins)[1]){
    if (dim(temp)[1] == 0){
      dfLarge = newCheckins
      S = 1
      max_id = newCheckinsReturn$response$pagination$max_id
      while (S == 1){
        print("Getting all checkins")
        # Woah more than 50 new beers!
        Sys.sleep(wTime)
        olderChecksRet = untappdAPI(method = method, param = user, query = list(limit = lim, max_id = max_id))
        max_id = olderChecksRet$response$pagination$max_id
        olderChecks = unpackCheckins(olderChecksRet)
        temp = row_match(cHist, olderChecks)
        dfLarge = rbind(dfLarge,olderChecks)
        if (dim(temp)[1] != 0 || nrow(olderChecks) != lim){
          cHist = rbind(cHist, dfLarge)
          S = 0
        }
      }
    } else {
      # New Checkins!
      cHist = rbind(cHist,newCheckins)
    }
    dups = duplicated(cHist$checkin_id)
    cHist = cHist[!dups,]
    cHist = cHist[order(cHist$checkin_id, decreasing = T),]
    saveRDS(cHist, file = paste0("checkinHist/",user,".rds") )
  } else {
    print("No new checkins!")
  }
  
  print("getting new venues")
  checkForNewVenues(cHist, wTime = wTime)
}
