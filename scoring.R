#
# 05-09-2018
# Some scoring algo.
#
# Ideas: Count: unique, badges.
#
# dep makedfVenue.R
#
#
makedfScore <- function(){
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
  #
  tot = getVenueCheckIn()
  
  # tot$created_at2 = as.Date( substr( tot$created_at,6, 16), format = "%d %b %Y")
  
  tot = subset(tot, !is.na(tot$venue_name))

  temp = unique(tot[,c("venue_id","venue_name")])
  temp = temp[!duplicated(temp$venue_id),]
  aggTot = aggregate(list(count = tot$venue_id), by = list(user_name = tot$user_name,
                                                           venue_id = tot$venue_id), length)
  aggTot = merge(aggTot, temp, by = "venue_id", all.x = T)
  spreadTot = spread(aggTot, key = user_name, value = count)
  spreadTot[is.na(spreadTot)] = 0
  #
  dfVenue = makedfVenue()
  spreadTot = merge(spreadTot,dfVenue, by = "venue_id", all.x = T)
  #
  score = na.omit(spreadTot)
  names(score)[names(score) == "lng"] = "lon"
  
  users = dir("checkinHist/")
  users = gsub("\\..*", "", users)
  val = vector(mode = "numeric", length = nrow(score))
  for (i in users){
    if (i %in% names(score)){
      const = getTeam(i,opts = "num")
      val = score[[i]]*const + val
    }
  }
  score$val = val
  
  # Add team colours
  score$col = getTeam(val = score$val)
  # score$col = "blue"
  # score$col[score$val>0] = "green"
  #
  return(score)
}

getTeam <- function(user = NULL, opts = NULL, val = NULL){
  if (!is.null(val)){
    team = ifelse(test = val>0, yes = "green", no = "blue")
    return(team)
  }
  if (is.null(user) & is.null(opts)){
    return()
  }
  teamList = list(
    blue = c("Slendrick", "knoe1703", "camillask"),
    green = c("Garbacz", "hellegskov")
  )
  if (identical(opts, "teams")){
    return(teamList)
  }
  
  team = useDict(user, teamList)
  if (identical(opts,"num")){
    team = ifelse(test = team == "green", yes = 1, no = -1)
  } 
  return(team)
}

