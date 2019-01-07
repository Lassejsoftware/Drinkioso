#
# 30-12-2018
# 
# user histogram plots
#
userHist <- function(user, simple = F, startDate, map, isBar = F){
  
  xlab = 13
  nBar = 10
  
  # Get data
  userDat = readRDS(paste0("checkinHist/", user, ".rds"))
  userInfo = readRDS(paste0("users/", user, ".rds"))
  joinDate = userInfo$joinDate
  if (is.null(joinDate)){
    joinDate = startDate
  }
  if (simple){
    userDat$beer_style = gsub("\\-.*", "", userDat$beer_style)
  }
  userDat$time = untappd2POSIXct(userDat$created_at)
  userDat = subset(userDat, userDat$time >= startDate & userDat$time >= joinDate)
  
  if (isBar){
    
  }
  
  dfVenue = makedfVenue(map = map)
  if (isBar){
    dfVenue = subset(dfVenue, dfVenue$isBar)
  }
  userDat = subset(userDat, userDat$venue_id %in% dfVenue$venue_id)
  
  userDat$is_venue = ifelse(!is.na(userDat$venue_id),"Yes", "No")
  userDat$is_venue = factor(userDat$is_venue, levels = sort(unique(userDat$is_venue), decreasing = F))
  
  teamCol = ifelse(getTeam(user = user) == "red", "#F8766D", "#619CFF")
  
  counts = count_unique(df = userDat, var = "beer_style")
  userDat = subset(userDat, userDat$beer_style %in% counts$beer_style[1:nBar])
  
  # Plot that stuff
  
  p <- ggplot(data = userDat) + geom_bar(aes(x = beer_style, fill = is_venue)) + 
    labs(title = paste0("Histogram of beer types for ", user), x = "", y = "Counts") +
    guides(fill=FALSE) +
    scale_fill_manual(values = c(No = "gray",
                                 Yes = teamCol)) +
    theme(
      axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = xlab),
      axis.text.y = element_text(size = xlab)
      )
  p
  return(p)
}
