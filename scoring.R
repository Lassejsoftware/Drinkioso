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
  
  # tot$created_at2 = as.Date( substr( tot$created_at,6, 16), format = "%d %b %Y")
  
  tot = subset(tot, !is.na(tot$venue_name))

  temp = unique(tot[,c("venue_id","venue_name")])
  temp = temp[!duplicated(temp$venue_id),]
  aggTot = aggregate(list(count = tot$venue_id), by = list(user_name = tot$user_name,
                                                           venue_id = tot$venue_id), length)
  aggTot = merge(aggTot, temp, by = "venue_id", all.x = T)
  spreadTot = spread(aggTot, key = user_name, value = count)
  spreadTot[is.na(spreadTot)] = 0
  #
  dfVenue = makedfVenue()
  spreadTot = merge(spreadTot,dfVenue, by = "venue_id", all.x = T)
  #
  score = na.omit(spreadTot)
  names(score)[names(score) == "lng"] = "lon"
  score$val = score$Garbacz + score$hellegskov- score$Slendrick - score$knoe1703 - score$camillask
  # saveRDS(spreadTot, file = "testMapDat.rds")
  #
  return(score)
}
