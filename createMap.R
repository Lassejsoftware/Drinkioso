#
# 14-09-2018
#
# Create map
#
createMap <- function(score, map, alpha = 0.5){
  
  # Only include locations on the map
  lims = attr(map,"bb")
  score = subset(score, score$lat>lims$ll.lat & score$lat<lims$ur.lat & score$lon>lims$ll.lon & score$lon<lims$ur.lon)
  
  # Extend a bit for reducing artifacts
  epsi = 0.001
  N = dim(score)[1]
  score[N+1,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ll.lat+epsi,0)
  score[N+2,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ur.lat-epsi,0)
  score[N+3,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ll.lat+epsi,0)
  score[N+4,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ur.lat-epsi,0)
  
  # make a map image
  mapImage = as.image(Z = score$val, x = score[,c("lon","lat")])
  mapImage$z[is.na( mapImage$z )] = 0
  
  # Apply integral kernel
  mapSmooth = applyFilter(x = mapImage$z, kernel = convKernel(sigma = 4.5, k = 'gaussian'))
  
  # Make smoothed score df
  scoreExp <- expand.grid(x = mapImage$x, y = mapImage$y)
  scoreExp$val <- as.vector(mapSmooth)
  scoreExp <- na.omit(scoreExp)
  
  # Calculate contour lines 
  BREAKS = c(seq(from = 0.5*min(scoreExp$val), to = 0.5*max(scoreExp$val), length.out = 8),-0.01,0.01)
  BREAKS = round(BREAKS*1000)/1000
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
  gg2
}
