#
# 05-09-2018
# Some scoring algo.
#
# Ideas: Count: unique, badges.
#
# dep makedfVenue.R
#
#
makedfScore <- function(){
  getVenueCheckIn <- function(){
    hists = dir("checkinHist/")
    cc = 1
    for (i in hists){
      if (cc == 1){
        tot = readRDS(paste0("checkinHist/",i))
        cc = cc + 1
      } else {
        temp = readRDS(paste0("checkinHist/",i))
        tot = rbind(tot,temp)
      }
    }
    return(tot)
  }
  #
  tot = getVenueCheckIn()
  tot = subset(tot, !is.na(tot$venue_name))
  temp = unique(tot[,c("venue_slug","venue_name")])
  aggTot = aggregate(venue_slug ~ user_name + venue_name + venue_id, data = tot, length)
  names(aggTot)[names(aggTot) == "venue_slug"] <- "count"
  aggTot = merge(aggTot, temp, by = "venue_name", all.x = T)
  spreadTot = spread(aggTot, key = user_name, value = count)
  spreadTot[is.na(spreadTot)] = 0
  #
  dfVenue = makedfVenue()
  spreadTot = merge(spreadTot,dfVenue, by = "venue_id", all.x = T)
  #
  score = na.omit(spreadTot)
  names(score)[names(score) == "lng"] = "lon"
  score$val = score$Garbacz + score$hellegskov - score$Slendrick - score$knoe1703
  # saveRDS(spreadTot, file = "testMapDat.rds")
  #
  return(score)
}
