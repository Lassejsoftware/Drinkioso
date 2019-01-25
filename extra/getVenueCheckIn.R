#
#
#
getVenueCheckIn <- function(team = F){
  if (!team){
    users = dir("checkinHist/")
  } else {
    users = getUsers(team = team)
    users = paste0(users, ".rds")
  }
  cc = 1
  for (i in users){
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
