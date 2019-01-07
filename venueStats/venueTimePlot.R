#
# 05-01-2018
#
# Makes a plot of when people visited a venue.
#
venueTimePlot <- function(venueId, startDate){
  
  xlab = 13
  
  venueInfo = readRDS(paste0("venues/", venueId, ".rds"))
  
  tot = getVenueCheckIn()
  tot = tot[!is.na(tot$venue_id),]
  tot = subset(tot, tot$venue_id == venueId)
  tot$time = untappd2POSIXct(tot$created_at)
  
  users = tot$user_name
  for (i in users){
    userInfo = readRDS(paste0("users/",i,".rds"))
    joinDate = userInfo$joinDate
    if (is.null(joinDate)){
      joinDate = startDate
    }
    tot = subset(tot, !(tot$user_name == i & tot$time<joinDate) )
  }
  
  if (nrow(tot)<3){
    return( ggplot() + labs(title = "Venue has seen too little action for a graph"))
  }
  
  tot = arrange(tot,time)
  tot$cc = 1:nrow(tot)
  tot$val = getTeam(user = tot$user_name, opts = "num")
  tot$teamNum = cumsum(tot$val)
  tot$team = proper(getTeam(val = tot$teamNum))
  tot$team[tot$team == "Black"] = "No owner"
  
  tot$team = factor(tot$team, levels = unique(tot$team))
  
  
  p <- ggplot(tot) + 
    geom_point(aes(x = time, y = cc, color = team, size = abs(teamNum))) +
    geom_line(aes(x = time, y = cc)) + 
    scale_color_manual(values = c(Red = "#F8766D",
                                  Blue = "#619CFF",
                                  "No owner" = "black")) +
    labs(title = paste0("Beers had at ", venueInfo$venue_name), x = "", y = "Number of beers", 
         color = "Owner", size = "Venue score") + 
    theme(
      axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = xlab),
      axis.text.y = element_text(size = xlab)
    )  

  return(p)

}
