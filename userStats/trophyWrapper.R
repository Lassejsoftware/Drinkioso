#
# 30-12-2018
#
# Wrapper for trophy calculations
#
trophyWrapper <- function(trophy = NULL, startDate, map, opts = NULL){
  trophies = c("Columbus", "Warrior")
  if (!is.null(opts)){
    if (opts == "trophies"){
      return(sort(trophies))
    }
  }
  switch (trophy,
    "Columbus" = calcColumbus(startDate = startDate, map = map, reCalc = reCalc),
    "Warrior" = calcWarrior(startDate = startDate, reCalc = reCalc)
  )

}