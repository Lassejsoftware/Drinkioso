#
# 30-12-2018
#
# Beer timeline for a user
#
userTimeline <- function(user, startDate, map, isBar = F, reCalc = F){
  
  xlab = 13
  teamCol = ifelse(getTeam(user = user) == "red", "#F8766D", "#619CFF")
  period = 3*30*24*60*60
  
  fileName = paste0("userStats/data/userTimeLine_", user, ".rds")
  
  # if (reCalc || !file.exists(fileName)){
    
    # Get data
    userDat = readRDS(paste0("../drinkiosoData/checkinHist/", user, ".rds"))
    userInfo = readRDS(paste0("../drinkiosoData/users/", user, ".rds"))
    joinDate = userInfo$joinDate
    if (is.null(joinDate)){
      joinDate = startDate
    }
    
    userDat$time = untappd2POSIXct(userDat$created_at)
    userDat = subset(userDat, userDat$time > startDate & userDat$time >= joinDate)
    
    dups = duplicated(userDat$checkin_id)
    userDat = userDat[!dups,]
    
    dfVenue = makedfVenue(map = map)
    if (isBar){
      dfVenue = subset(dfVenue, dfVenue$isBar)
    }
    userDat = subset(userDat, userDat$venue_id %in% dfVenue$venue_id)
    
    userDat$num = nrow(userDat):1
    userDat$is_venue = ifelse(!is.na(userDat$venue_id),"Yes", "No")
    userDat$is_venue = factor(userDat$is_venue, levels = sort(unique(userDat$is_venue), decreasing = T)) # ggplot doesnt really play nice with string comparisons.
    
    userDat$timeNum = as.numeric(userDat$time)
  #   
  #   saveRDS(userDat, fileName)
  #   
  # } else {
  #   userDat <- readRDS(fileName)
  # }
  
  userDat$timeNum = userDat$timeNum - userDat$timeNum[1]
  userDat = subset(userDat, userDat$timeNum > - period)
  
  
  p <- ggplot(userDat) + 
    geom_line(aes(x = time, y = num), size = 1.5) + 
    geom_point(aes(x = time, y = num, color = is_venue), size = 4) + # 
    scale_color_manual( values = c(Yes = teamCol, 
                                   No = "gray")) +
    guides(color = F) +
    labs(title = paste0("Beer timeline for ", user), 
         x="", y = "Beer number") + # , color = "Venue"
    theme(
      axis.text.x = element_text(size = xlab),
      axis.text.y = element_text(size = xlab)
    )
  p
  return(p)
}
