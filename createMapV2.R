#
# 14-09-2018
#
# Create map
#
createMap <- function(score, map, alpha = 0.5, sigma = 3){
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
      if (S[i]>0){
        scoreMat = scoreMat + round(S[i]*exp(-lambda*rMat))
      } else {
        scoreMat = scoreMat + round(S[i]*exp(-lambda*rMat))
      }
    }
    return(scoreMat)
  }
  
  # Only include locations on the map
  lims = attr(map,"bb")
  score = subset(score, score$lat>lims$ll.lat & score$lat<lims$ur.lat & score$lon>lims$ll.lon & score$lon<lims$ur.lon)
  outList$score = score
  
  # Extend a bit for reducing artifacts
  epsi = 0.001
  N = dim(score)[1]
  score[N+1,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ll.lat+epsi,0)
  score[N+2,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ur.lat-epsi,0)
  score[N+3,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ll.lat+epsi,0)
  score[N+4,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ur.lat-epsi,0)
  
  # make a map image
  nx = 1024
  x = seq(min(score$lon), max(score$lon), length.out = nx)
  y = seq(min(score$lat), max(score$lat), length.out = nx)
  x0 = score$lon
  y0 = score$lat
  S = score$val
  mapImage = calcMap(x=x,
                     y=y,
                     x0=x0,
                     y0=y0,
                     S=S,
                     lambda = 400)
  
  # Make smoothed score df
  scoreExp <- expand.grid(x = x, y = y)
  scoreExp$val <- as.vector(mapImage)
  scoreExp <- na.omit(scoreExp)
  tscoreExp <<- scoreExp
  # Calculate contour lines 
  # BREAKS = c(seq(from = 0.5*min(scoreExp$val), to = 0.5*max(scoreExp$val), length.out = 8),-0.01,0.01)
  # BREAKS = round(BREAKS*1000)/1000
  
  BREAKS = c(seq(-50,50,by=10),-0.1,0.1)
  
  # Make a map.
  gg <- ggmap(map,
              extent = "device", # 
              ylab = "Latitude",
              xlab = "Longitude")
  gg2 <- gg +
    stat_contour(data = scoreExp, breaks = BREAKS,
                 aes(x=x,y=y,z=val, fill = factor(..level..)), geom = "polygon", alpha = alpha) +
    scale_fill_viridis(discrete = T) +
    geom_point(data = score,
               aes(x = lon, y = lat, size = abs(val), color = sign(val))) 
  
  outList$plot <- gg2
  return(outList)
}
