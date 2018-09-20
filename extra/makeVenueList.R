#
# 20-09-2018
#
# Makes a list of venues for the selecter in venue stats.
# Should be extended with relevant stats as well.
#
makeVenueList <- function(){
  venues = dir("venues")
  venueList = list()
  temp = c()
  for (i in venues){
    vDat = readRDS(paste0("venues/",i))
    temp = c(temp, vDat$venue_name)
  }
  venueList$names = temp
  return(venueList)
}