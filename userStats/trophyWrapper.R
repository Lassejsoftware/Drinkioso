#
# 30-12-2018
#
# Wrapper for trophy calculations
#
trophyWrapper <- function(trophy = NULL, opts = NULL){
  if (!is.null(opts)){
    if (opts == "trophies"){
      trophies = c("Columbus", "Warrior")
      return(sort(trophies))
    }
  }
}