#
# 08-11-2018
#
# Make a leaflet map of cph with bars
#
# map is basically only for limiting georgraphy of bars.
#
createMap <- function(score, lambda = 650, noTeam = NULL, nx = 200){
  outList = list()
  
  calcMap <- function(x, y, x0, y0, S, lambda){
    # x,y: grid of map.
    # x0,y0 vectors of bar coords.
    # S bar score.
    # lambda = decay const.
    if (length(x0) != length(y0) | length(x0) != length(S)){
      print("All bars should have a individual location and score")
      return()
    }
    # 
    nx = length(x)
    ny = length(y)
    # generate grid
    g = expand.grid(x = x, y = y)
    temp = c(g$x)
    xMat = matrix(data = temp, nrow = nx, ncol = ny)
    temp = c(g$y)
    yMat = matrix(data = temp, nrow = nx, ncol = ny)
    #
    scoreMat = matrix(data = 0, nrow = length(x), ncol = length(y))
    for (i in 1:length(x0)){
      rMat = sqrt((xMat - x0[i])^2 + (yMat - y0[i])^2)
      scoreMat = scoreMat + S[i]*exp(-lambda*rMat)
    }
    return(scoreMat)
  }
  
  # Only include bars in Copenhagen.
  # lims = attr(map,"bb")
  # score = subset(score, score$lat>lims$ll.lat & score$lat<lims$ur.lat & score$lon>lims$ll.lon & score$lon<lims$ur.lon)
  
  # Make labels
  users = getUsers(team = T)
  team = proper(score$col)
  inds = which(names(score) %in% users)
  drinker = c()
  dTeam = c()
  num = c()
  totBeer = c()
  for (i in 1:dim(score)[1]){ # Make better solution please
    drinker[i] = names(which.max(score[i,inds]))
    num[i] = max(score[i,inds])
    dTeam[i] = proper(getTeam(user = drinker[i]))
  }
  totBeer = rowSums(score[,users])
  
  label = paste0("<b> Venue: </b>", score$venue_name, " owned by team <b>", team,  "</b> <br/>",
                 "<b> Venue score is:  </b>", abs(score$val) , "<br/>",
                 "<b> Greatest beer drinker: </b>", drinker, " from team <b>", dTeam, "</b> with ", num, " beers! <br/>",
                 "<b> Beers consumed at venue: </b>", totBeer
  )
  vec = grep("Black", label)
  label[vec] = paste0("<b> Venue: </b>", score$venue_name[vec], " is not owned by any team! <b>Go drink some beers!</b> <br/>",
                      "<b> Greatest beer drinker: </b>", drinker[vec], " from team <b>", dTeam[vec], "</b> with ", num[vec], " beers!"
  )
  score$label = label
  
  # Will mess up the map!
  if (!is.null(noTeam)){
    score$val[vec] = noTeam
  }
  outList$score = score
  # make a map image
  dx = abs(min(score$lon)- max(score$lon))
  dy = abs(min(score$lat)- max(score$lat))
  #
  x = seq(min(score$lon)-0.75*dx, max(score$lon)+0.75*dx, length.out = nx)
  y = seq(min(score$lat)-0.75*dy, max(score$lat)+0.75*dy, length.out = nx)
  x0 = score$lon
  y0 = score$lat
  S = score$val
  mapImage = calcMap(x=x,
                     y=y,
                     x0=x0,
                     y0=y0,
                     S=S,
                     lambda = lambda)
  
  epsi = 0.01#0.05
  mapImage[abs(mapImage)<epsi] = 0
  
  outList$mapImage = mapImage
  
  # Make smoothed score df
  scoreExp <- expand.grid(x = x, y = y)
  scoreExp$val <- as.vector(mapImage)
  scoreExp <- na.omit(scoreExp)
  outList$scoreExp <- scoreExp
  
  return(outList)
}
