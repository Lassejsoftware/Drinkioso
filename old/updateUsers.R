#
# 29-08-2018
#
# Update scripts which downloads latest beer list.
#
# Todo: investigate flatten in fromJSON
#
.libPaths("c:/R_libs")
library(jsonlite)
#
rm(list = ls())
# API setup
id = "562203AF6B431C6653E6E7346ACB17FCB6AC5F14"
secret = "54E07A46395CE64ADB485888EEAE297D1B9D17FF"
link = "https://api.untappd.com/v4/"
method = "user/info/"
#
wTime = 5*60 # Wait time between api calls and calculations. Only 100 calls per hour is allowed.
path = "C:/Users/jqmt/Desktop/jens/untappd/"
setwd(path)
#
teamList = list(yellow = c("Garbacz","hellegskov"),
                red = c("Slendrick"))
#
users = unname(unlist(teamList))
#
files = dir("data")
for (i in users){
  final = paste0(link,method,i,"?client_id=",id,"&client_secret=",secret)
  userInfo = fromJSON(final)
  if (userInfo$meta$code == 200){ # Check if everything went alright
    data = userInfo$response$user$checkins$items
    if (i %in% files){
      temp = readRDS(paste0("data/",i))
      
    }
  }
}
#
i = users[3]
method = "user/checkins/"
final = paste0(link,method,i,"?client_id=",id,"&client_secret=",secret,"&limit=50")
userCheckins = fromJSON(final)
#
items = userCheckins$response$checkins$items
# Get the simple variables
# varsSimple = c("created_at", "checkin_comment", "rating_score", "user", "beer")
vars = list(created_at = c(),
            checkin_comment = c(),
            rating_score = c(),
            user = c(),
            beer = c(),
            brewery = c("brewery_id", "brewery_name", "brewery_active"),
            badges = c("count")
            )
dfTot = data.frame(checkin_id = items$checkin_id)
for (i in names(vars)){
  temp = items[[i]]
  if (length(vars[[i]]) != 0 ){ 
    temp = temp[,vars[[i]]]
  } 
  # Well i suppose we could do some merge magic instead?
  namesTot = names(dfTot)
  namesTemp = names(temp)
  if ("items" %in% namesTemp){
    print(paste0(i," does not seem to be simple"))
    a=b
  }
  if (length(namesTemp) == 0){
    if (!(i %in% namesTot)){
      dfTemp = data.frame(temp)
      names(dfTemp) <- i
      dfTot = cbind(dfTot,dfTemp)
    }
  } else {
    print(paste0("Throwing away", intersect(namesTemp,namesTot)))
    temp = temp[,setdiff(namesTemp,namesTot)]
    dfTot = cbind(dfTot,temp)
  }
}
# venue is a bit special.
temp = items$venue
venVars = c("venue_id", "venue_slug")
getFunc <- function(lst,n1,n2 = NULL){
  if (is.null(n2)){
    out = lst[[n1]]
    if (is.null(out)){
      out = NA
    }
  } else {
    out = lst[[n1]][[n2]]
    if (is.null(out)){
      out = NA
    }
  }
  return(out)
}
for (i in venVars){
  temp2 = unlist(lapply(temp, getFunc,n1=i))
  dfTemp = data.frame(temp2)
  names(dfTemp) <- i
  dfTot = cbind(dfTot,dfTemp)
}
# Time to check for new venues





temp3 = unlist(lapply(temp, func, n1 = "location", n2 = "lat"))



method = "user/beers/"
final = paste0(link,method,i,"?client_id=",id,"&client_secret=",secret)
userBeers = fromJSON(final)
#
library(httr)
i = users[1]
method = "user/checkins/"
final = paste0(link,method,i,"?client_id=",id,"&client_secret=",secret)
test = GET(final)
#
max_id = userCheckins$response$pagination$max_id
method = "v4/user/checkins/"
test2 = GET("https://api.untappd.com/",path = paste0(method,i), 
            query = list(
              client_id = id,
              client_secret = secret,
              max_id = max_id
            ))

test2$status_code
test2$url

final
