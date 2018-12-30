#
# 30-12-2018
#
# Beer timeline for a user
#
userTimeline <- function(user){
  
  xlab = 13
  
  # Get data
  userDat = readRDS(paste0("checkinHist/", user, ".rds"))
  
  dups = duplicated(userDat$checkin_id)
  userDat = userDat[!dups,]
  
  userDat$num = nrow(userDat):1
  userDat$is_venue = ifelse(!is.na(userDat$venue_id),"Yes", "No")
  
  # Fins time.
  time = as.character(userDat$created_at)
  time = substr(time,6,25)
  if (is.na(strptime("01 oct","%d %b"))){
    time = gsub("oct", "okt",time,ignore.case = T)
    time = gsub("may", "maj",time,ignore.case = T)
  }
  userDat$time = as.POSIXct(strptime(time, "%d %b %Y %H:%M:%S")) 
  
  p <- ggplot(userDat) + 
    geom_line(aes(x = time, y = num), size = 1.5) + 
    geom_point(aes(x = time, y = num, color = is_venue), size = 4) + 
    labs(title = paste0("Beer timeline for ", user,"\nOnly shows beers tracked in Drinkioso."), 
         x="", y = "Beer number", color = "Venue") + 
    theme(
      axis.text.x = element_text(size = xlab),
      axis.text.y = element_text(size = xlab)
    )
  
}
