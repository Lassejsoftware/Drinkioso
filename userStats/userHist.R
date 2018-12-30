#
# 30-12-2018
# 
# user histogram plots
#
userHist <- function(user, simple = F){
  
  xlab = 13
  
  # Get data
  userDat = readRDS(paste0("checkinHist/", user, ".rds"))
  if (simple){
    userDat$beer_style = gsub("\\-.*", "", userDat$beer_style)
  }
  
  userDat$is_venue = ifelse(!is.na(userDat$venue_id),"Yes", "No")
  
  # Plot that stuff
  
  p <- ggplot(data = userDat) + geom_bar(aes(x = beer_style, fill = is_venue)) + 
    labs(title = paste0("Histogram of beer types for ", user), x = "", y = "Counts", fill = "Venue" ) + 
    theme(
      axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5, size = xlab),
      axis.text.y = element_text(size = xlab)
      )
  
  return(p)
  
}
