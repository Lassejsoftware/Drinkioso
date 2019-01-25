#
# 30-12-2018
#
# Wrapper for user plots
#
userPlotWrapper <- function(user = NULL, plotType = NULL, startDate, map = NULL, opts = NULL, reCalc = F){
  if (!is.null(opts)){
    if (opts == "plots"){
      plots = c("Style histogram",
                "Beer timeline")
      return(plots)
    }
  }
  
  p <- switch(plotType,
              "Style histogram" = userHist(user = user, startDate = startDate, map = map, reCalc = reCalc),
              "Beer timeline" = userTimeline(user = user, startDate = startDate, map = map, reCalc = reCalc)
              )
  
  return(p)
}