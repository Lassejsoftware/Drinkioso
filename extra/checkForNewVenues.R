#
# 03-09-2018
#
# Checks a checkins data frame to see if it includes new venues and finds their info.
#
# dep; makeVenueNames
#
checkForNewVenues <- function(df, overWrite = 0, wTime = 2*60){
  method = "venue/info/"
  venuesOld = dir("venues/")
  venuesNew = unique(makeVenueNames(df))
  if (overWrite == 0){
    venuesNew = subset(venuesNew, !(venuesNew$name %in% venuesOld) )
  }
  if (is.null(venuesNew)){print("No new venues");return()}
  if (dim(venuesNew)[1]==0){print("No new venues");return()}
  for (i in 1:dim(venuesNew)[1]){
    print("Trying to grap new venues")
    venueInfo = untappdAPI(method = method, param = venuesNew$venue_id[i])
    listVenue = venueInfo$response$venue
    print(paste0("Saving venue ", venuesNew$venue_slug[i]))
    saveRDS(listVenue, file = paste0("venues/", venuesNew$name[i]))
    print("Waiting for next call")
    print(paste0(dim(venuesNew)[1] - i," venues left."))
    Sys.sleep(wTime)
  }
}
