#
# 30-12-2018
#
# Wrapper for user plots
#
userPlotWrapper <- function(user = NULL, plotType = NULL, startDate, opts = NULL){
  if (!is.null(opts)){
    if (opts == "plots"){
      plots = c("Style histogram - simple", 
                "Style histogram",
                "Beer timeline")
      return(plots)
    }
  }
  
  p <- switch(plotType,
              "Style histogram" = userHist(user = user, startDate = startDate),
              "Style histogram - simple" = userHist(user = user, simple = T, startDate = startDate),
              "Beer timeline" = userTimeline(user = user, startDate = startDate)
              )
  
  return(p)
}