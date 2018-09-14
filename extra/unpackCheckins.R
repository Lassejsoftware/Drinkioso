#
# 03-09-2018
# unpack the respons from user/checkins/ to simple data.frame
#
#
unpackCheckins <- function(returnDat){
  # Some checks to be certain that returnDat is as expected
  #
  items = returnDat$response$checkins$items # Get the data
  #
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
  # This is hopefully universal! Stuff I think could be usefull for scoring
  vars = list(created_at = c(),
              checkin_comment = c(),
              rating_score = c(),
              user = c("uid", "user_name", "location"),
              beer = c("bid", "beer_name", "beer_style", "beer_slug", "beer_abv", "beer_active", "has_had"),
              brewery = c("brewery_id", "brewery_name", "brewery_active"),
              badges = c("count")
  )
  venVars = c("venue_id", "venue_name", "venue_slug")
  # Lets start
  dfTot = data.frame(checkin_id = items$checkin_id) # make data frame
  for (i in names(vars)){
    temp = items[[i]]
    if (length(vars[[i]]) != 0 ){ 
      temp = temp[,vars[[i]]]
    } 
    # I suppose we could do some merge magic instead?
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
      # print(paste0("Throwing away ", intersect(namesTemp,namesTot)))
      temp = temp[,setdiff(namesTemp,namesTot)]
      dfTot = cbind(dfTot,temp)
    }
  }
  # Venue is a bit special
  temp = items$venue
  if (!is.data.frame(temp)){
    for (i in venVars){
      temp2 = unlist(lapply(temp, getFunc,n1=i))
      dfTemp = data.frame(temp2)
      names(dfTemp) <- i
      dfTot = cbind(dfTot,dfTemp)
    }
  } else {
    dfTot = cbind(dfTot,temp[,venVars])
  }
  #
  return(dfTot)
}