#
# 10-09-2018
#
# Make a df with venue info. This is nothing but a draft.
#
makedfVenue <- function(map = NULL){
  venues = dir("venues")
  dfVenue = data.frame()
  cc = 1
  for (i in venues){
    temp = readRDS(paste0("venues/",i))
    #
    dfVenue[cc, "venue_id"] = temp$venue_id
    dfVenue[cc, "total_count"] = temp$stats$total_count
    dfVenue[cc, "total_user_count"] = temp$stats$total_user_count
    dfVenue[cc, "lat"] = temp$location$lat
    dfVenue[cc, "lng"] = temp$location$lng
    dfVenue[cc, "isBar"] = sum(grepl(pattern = "bar|pub|Brewery|Gastropub|Beer|restaurant", x = temp$categories, ignore.case = T))>0
    #
    cc = cc + 1
  }
  names(dfVenue)[names(dfVenue) == "lng"] = "lon" 
  if (!is.null(map)){
    # This only work for some cph map.
    lims = attr(map,"bb")
    dfVenue = subset(dfVenue, dfVenue$lat>lims$ll.lat & dfVenue$lat<lims$ur.lat & dfVenue$lon>lims$ll.lon & dfVenue$lon<lims$ur.lon)
  }
  return(dfVenue)
}


# venues = dir("venues")
# for (i in venues){
#   temp = readRDS(paste0("venues/",i))
#   print(temp$categories$items$category_name)
#   Sys.sleep(2)
# }
