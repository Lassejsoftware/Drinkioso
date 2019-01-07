#
# 31-12-2018
#
# Wrapper for team plots
#
teamPlotWrapper <- function(plotType = NULL, startDate = NULL, score = NULL, map = NULL, recalc = F, opts = NULL){
  if (!is.null(opts)){
    if (opts == "plots"){
      teamPlots = c("Venues over time", 
                    "Venue dominance")
      return(teamPlots)
    }
  }
  
  p <- switch(plotType,
              "Venues over time" = teamTimeline(startDate = startDate, map = map, recalc = recalc),
              "Venue dominance" = teamHist(score = score, startDate = startDate)
  )
  
  return(p)
  
}
