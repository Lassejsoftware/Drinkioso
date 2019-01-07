#
#
#
getVenueCheckIn <- function(){
  hists = dir("checkinHist/")
  cc = 1
  for (i in hists){
    if (cc == 1){
      tot = readRDS(paste0("checkinHist/",i))
      cc = cc + 1
    } else {
      temp = readRDS(paste0("checkinHist/",i))
      tot = rbind(tot,temp)
    }
  }
  return(tot)
}
