#
# 14-01-2018
#
# Calc warrior trophy
# 
# Todo: consider if venue take over can be given to multiple if they are at the same place?
#
calcWarrior <- function(startDate, map, reCalc = F, venFilt = F){
  parVec = c(cTrophy = 5, minTot = 10)
  if (opts == "pars"){
    return(parVec)
  }
  
  fileName = "userStats/data/warrior.rds"
  if (reCalc || !file.exists(fileName)){
    tot = getVenueCheckIn(team = T)
    dfVenue = makedfVenue(map = map)
    tot = subset(tot, tot$venue_id %in% dfVenue$venue_id)
    
    teamVec = proper(names(getTeam(opts = "teams")))
    
    war = data.frame(user_name = "start", venue_name = "start")
    cc = 1
    for (i in dfVenue$venue_id){
      print(cc)
      cc = cc + 1
      if (venFilt){
        # Use a filter on venue category.
      }
      tempList = venueTimePlot(startDate = startDate, venueId = i, tot = tot)
      tempDat = tempList$data 
      vec = which(!(tempDat$team %in% teamVec))
      if (length(vec) != 0 && nrow(tempDat) > parVec["minTot"]){
        for (j in vec){
          takeOver = as.character(tempDat$team[(j-1):(j+1)])
          if (length(unique(takeOver))>2){ # It is a real take over!
            war = rbind(war, unique(tempDat[j:(j+1),c("user_name","venue_name")]) )
          }
        }
      }
    }
    war = war[-1,]
    
    dfCount = count_unique(war, "user_name")
    dfCount$trophy = floor(dfCount$N/parVec["cTrophy"])
    
  saveRDS(dfCount, fileName)
  
  } else {
    dfCount <- readRDS(fileName)
  }
  
  return(dfCount)
  
}