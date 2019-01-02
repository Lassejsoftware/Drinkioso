#
# 30-12-2018
# 
# user histogram plots
#
userHist <- function(user, simple = F, startDate){
  
  xlab = 13
  nBar = 10
  
  # Get data
  userDat = readRDS(paste0("checkinHist/", user, ".rds"))
  if (simple){
    userDat$beer_style = gsub("\\-.*", "", userDat$beer_style)
  }
  userDat$time = untappd2POSIXct(userDat$created_at)
  userDat = subset(userDat, userDat$time > startDate)
  
  userDat$is_venue = ifelse(!is.na(userDat$venue_id),"Yes", "No")
  
  counts = count_unique(df = userDat, var = "beer_style")
  userDat = subset(userDat, userDat$beer_style %in% counts$beer_style[1:nBar])
  
  # Plot that stuff
  
  p <- ggplot(data = userDat) + geom_bar(aes(x = beer_style, fill = is_venue)) + 
    labs(title = paste0("Histogram of beer types for ", user), x = "", y = "Counts", fill = "Venue" ) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = xlab),
      axis.text.y = element_text(size = xlab)
      )
  
  return(p)
  
}
