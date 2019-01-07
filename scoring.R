#
# 05-09-2018
# Some scoring algo.
#
# Ideas: Count: unique, badges.
#
# dep makedfVenue.R
#
#
makedfScore <- function(startDate = NULL, map = NULL, isBar = F){
  #
  tot = getVenueCheckIn()
  
  # tot$created_at2 = as.Date( substr( tot$created_at,6, 16), format = "%d %b %Y")
  
  tot = subset(tot, !is.na(tot$venue_name))
  tot$time = untappd2POSIXct(tot$created_at)
  # Start date of drinkioso
  print(dim(tot))
  if (class(startDate)[1] == "POSIXct"){
    tot = subset(tot, tot$time > startDate)
  }
  print(startDate)
  print(dim(tot))
  # Join date for user
  users = getUsers()
  for (i in users){
    userInfo = readRDS(paste0("users/",i,".rds") )
    joinDate = userInfo$joinDate
    if (!is.null(joinDate)){
      vec = tot$user_name == i & tot$time<joinDate
      tot = tot[!vec,]
    }
  }

  temp = unique(tot[,c("venue_id","venue_name")])
  temp = temp[!duplicated(temp$venue_id),]
  aggTot = aggregate(list(count = tot$venue_id), by = list(user_name = tot$user_name,
                                                           venue_id = tot$venue_id), length)
  aggTot = merge(aggTot, temp, by = "venue_id", all.x = T)
  spreadTot = spread(aggTot, key = user_name, value = count)
  spreadTot[is.na(spreadTot)] = 0
  #
  dfVenue = makedfVenue(map = map)
  spreadTot = merge(spreadTot,dfVenue, by = "venue_id", all.x = T)
  #
  score = na.omit(spreadTot)
  if (isBar){
    score = subset(score, score$isBar)
  }
  names(score)[names(score) == "lng"] = "lon"
  
  val = vector(mode = "numeric", length = nrow(score))
  for (i in users){
    if (i %in% names(score)){
      const = getTeam(i,opts = "num")
      val = score[[i]]*const + val
    }
  }
  score$val = val
  
  # Add team colours
  score$col = getTeam(val = score$val)
  #
  
  
  return(score)
}



