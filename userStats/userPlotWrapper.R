#
# 30-12-2018
#
# Wrapper for user plots
#
userPlotWrapper <- function(user = NULL, plotType = NULL, startDate, map = NULL, opts = NULL){
  if (!is.null(opts)){
    if (opts == "plots"){
      plots = c("Style histogram", 
                #"Style histogram - all",
                "Beer timeline")
      return(plots)
    }
  }
  
  p <- switch(plotType,
              "Style histogram - all" = userHist(user = user, startDate = startDate),
              "Style histogram" = userHist(user = user, simple = T, startDate = startDate, map = map),
              "Beer timeline" = userTimeline(user = user, startDate = startDate, map = map)
              )
  
  return(p)
}