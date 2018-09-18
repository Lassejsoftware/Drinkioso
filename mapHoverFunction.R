#
# 15-09-2018
#
# Hover for the map.
#
mapHoverFunc <- function(hover, data = NULL){
  req(hover)
  # Lets figure out where that text box should be!
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)    
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)  
  #
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)    
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)   
  #
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 40, "px; top:", top_px + 40, "px;")
  #
  point <- nearPoints(data, hover, xvar = "lon", yvar = "lat", threshold = 5, maxpoints = 1, addDist = TRUE)
  if (dim(point)[1] == 0 ){return()} # Nothing nearby
  team = ifelse(point$val>0, "Yellow", "Blue")
  venue = point$venue_name
  score = abs( point$val )
  users = dir("checkinHist/")
  users = gsub("\\..*", "", users)
  inds = which(names(point) %in% users)
  drinker = names(which.max(point[,inds]))
  num = point[inds[ which.max(point[,inds])]]
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Venue: </b>", venue, " owned by team <b>", team,  "</b> <br/>",
                  "<b> Venue score is:  </b>", score , "<br/>",
                  "<b> Best team player: </b>", drinker, " with ", num, " beers!"
                  )
           )
      )
  )
}