#
# 14-01-2019
# Calc columbus trophy
#
#
calcColumbus <- function(cTrophy = 10, startDate, map, reCalc = F, venFilt = F){
  
  fileName = "userStats/data/columbus.rds"
  
  if (reCalc || !file.exists(fileName)) {
    tot = getVenueCheckIn()
    tot = subset(tot, !is.na(tot$venue_id))
    tot$time = untappd2POSIXct(tot$created_at)
    users = getUsers(team = T)
    tot = subset(tot, tot$time > startDate & tot$user_name %in% users)
    
    dfVenue = makedfVenue(map = map)
    
    if (venFilt){
      # Use some selection of categories which are alright. Perhaps multiplier > 0.5
    }
    tot = plyr::join(tot,dfVenue, by = "venue_id")
    tot = tot[complete.cases(tot), ]
    
    # Join date for user
    for (i in users){
      userInfo = readRDS(paste0("users/",i,".rds") )
      joinDate = userInfo$joinDate
      if (!is.null(joinDate)){
        vec = tot$user_name == i & tot$time<joinDate
        tot = tot[!vec,]
      }
    }
    
    tot = arrange(tot, venue_id, time)
    
    dups = duplicated(tot$venue_id)
    tot = subset(tot, !dups)
    
    dfCol = count_unique(tot, "user_name")
    dfCol$trophy = floor(dfCol$N/cTrophy)
    
    save(dfCol, fileName)
  } else {
    dfCol = readRDS(fileName)
  }
  
  return(dfCol)
}

