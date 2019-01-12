#
# 10-09-2018
#
# Make a df with venue info. This is nothing but a draft.
#
makedfVenue <- function(map = NULL){
  venues = dir("venues")
  dfVenue = data.frame()
  cc = 1
  defaultVal = 0.5
  # From low to high
  multiplyer = list("Outdoors|park|beach|Plaza|Racetrack|River|Train" = 0.25, 
                    "residence|Home" = 0.5,
                    "bar|pub|gastropub|beer|restaurent|Lounge|Bistro|Cocktail|Bodega|Hotel|Nightclub|Resort|Winery" = 1,
                    "Brewery" = 2
                    )
  
  for (i in venues){
    temp = readRDS(paste0("venues/",i))
    #
    dfVenue[cc, "venue_id"] = temp$venue_id
    dfVenue[cc, "total_count"] = temp$stats$total_count
    dfVenue[cc, "total_user_count"] = temp$stats$total_user_count
    dfVenue[cc, "lat"] = temp$location$lat
    dfVenue[cc, "lng"] = temp$location$lng
    dfVenue[cc, "isBar"] = sum(grepl(pattern = "bar|pub|Brewery|Gastropub|Beer|restaurant", x = temp$categories, ignore.case = T))>0
    if (!is.null(temp$categories$items$category_name[1])){
      dfVenue[cc, "category"] = temp$categories$items$category_name[1]
    }
    #
    for (j in names(multiplyer)){
      if (sum(grepl(pattern = j, x = temp$categories, ignore.case = T))>0){
        dfVenue[cc, "multiplyer"] = multiplyer[[j]]
      }
    }
    cc = cc + 1
  }
  names(dfVenue)[names(dfVenue) == "lng"] = "lon" 
  if (!is.null(map)){
    # This only work for some cph map.
    lims = attr(map,"bb")
    dfVenue = subset(dfVenue, dfVenue$lat>lims$ll.lat & dfVenue$lat<lims$ur.lat & dfVenue$lon>lims$ll.lon & dfVenue$lon<lims$ur.lon)
  }
  dfVenue$multiplyer[is.na(dfVenue$multiplyer)] = defaultVal
  
  dfVenue$multiplyer[dfVenue$venue_id == 2339990] = defaultVal 
  
  return(dfVenue)
}


# venues = dir("venues")
# for (i in venues){
#   temp = readRDS(paste0("venues/",i))
#   print(temp$categories$items$category_name)
#   Sys.sleep(2)
# }
# bleh <- function(i){
#   temp = readRDS(paste0("venues/",i))
#   temp$categories$items$category_name
# }
# test = lapply(venues, bleh) %>% unlist %>% unique %>% sort
# test = data.frame(cats = test)

# t1 = Sys.time()
# test = makedfVenue(map = cph)
# t2 = Sys.time()
# t2 - t1
