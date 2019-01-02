#
# 31-12-2018
#
# Number of bars over time for different teams
#
# dep: dplyr
#
teamTimeline <- function(startDate, recalc = F){
  
  
  xlab = 13
  file = "teamStats/timeSeries.rds"
  
  if (!file.exists(file) || recalc){
    
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
    
    tot = getVenueCheckIn()
    tot$team = getTeam(user = tot$user_name)
    tot$time = untappd2POSIXct(tot$created_at)
    tot = subset(tot, tot$time>startDate & !is.na(tot$venue_id))
    
    # Only include venues in Copenhagen.
    dfVenue = makedfVenue()
    tot = merge(tot,dfVenue, by = "venue_id", all.x = T)
    
    tot = subset(tot, tot$isBar)
    
    names(tot)[names(tot) == "lng"] = "lon"
    lims = attr(cph,"bb")
    tot = subset(tot, tot$lat>lims$ll.lat & tot$lat<lims$ur.lat & tot$lon>lims$ll.lon & tot$lon<lims$ur.lon)
  
    tot = arrange(tot,time)
    venues = unique(tot$venue_id)
    delta = 7*60*60*24
    breaks = seq(from = startDate, to = Sys.time() + delta, by = delta)
    tot$timeCut = cut(tot$time, breaks = breaks)
    dfRes = data.frame(team = "start", N = 0, timeCut = "start")
    
    for (i in unique(tot$timeCut)){
      print(i)
      indMax = max(which(tot$timeCut == i))
      temp = tot[1:indMax,]
      aggC = aggregate(checkin_id ~ venue_id + team, data = temp, length)
      dup = duplicated(aggC[,c("venue_id","checkin_id")]) + duplicated(aggC[,c("venue_id","checkin_id")], fromLast = T) == 1
      aggC = aggC[!dup,]
      teamVens = aggC %>% group_by(venue_id) %>% filter(checkin_id == max(checkin_id))
      cc = count_unique(teamVens, "team")
      cc$timeCut = i
      dfRes = rbind(dfRes,cc)
    }
    dfRes = dfRes[-1,]
    dfRes$time2 = as.POSIXct(strptime(dfRes$timeCut, "%Y-%m-%d %H:%M:%S"))
    
    dfRes$team = proper(dfRes$team)
    
    saveRDS(dfRes, file)
  } else {
    dfRes = readRDS(file)
  }
  
  dfRes$team2 = factor(dfRes$team, levels = unique(dfRes$team))
  
  p <- ggplot(dfRes) + 
    geom_point(aes(x = time2, y = N, color = team2), size = 4) + 
    labs(x = "", y = "Number of bars", title = "Number of venues owned by the different teams", color = "Team") +
    theme(
      axis.text.x = element_text(size = xlab),
      axis.text.y = element_text(size = xlab)
    )
  
  p
  return(p)
  
}

# Testing
# source("helperfunctions.R")
# source("extra/getTeam.R")
# startDate = as.POSIXct("01 jan 2018", format = "%d %b %Y")
# cph <<- readRDS("maps/cph.rds")
# teamTimeline(startDate)
