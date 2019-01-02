#
# 30-12-2018
#
# Beer timeline for a user
#
userTimeline <- function(user, startDate){
  
  xlab = 13
  
  # Get data
  userDat = readRDS(paste0("checkinHist/", user, ".rds"))
  
  userDat$time = untappd2POSIXct(userDat$created_at)
  userDat = subset(userDat, userDat$time > startDate)
  
  dups = duplicated(userDat$checkin_id)
  userDat = userDat[!dups,]
  
  userDat$num = nrow(userDat):1
  userDat$is_venue = ifelse(!is.na(userDat$venue_id),"Yes", "No")
  
  p <- ggplot(userDat) + 
    geom_line(aes(x = time, y = num), size = 1.5) + 
    geom_point(aes(x = time, y = num, color = is_venue), size = 4) + 
    labs(title = paste0("Beer timeline for ", user,"\nOnly shows beers tracked in Drinkioso."), 
         x="", y = "Beer number", color = "Venue") + 
    theme(
      axis.text.x = element_text(size = xlab),
      axis.text.y = element_text(size = xlab)
    )
  return(p)
}
