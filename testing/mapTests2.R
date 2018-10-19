#
# 10-09-2018
# The second map test.
#
.libPaths("c:/R_libs")
setwd("C:/Users/jqmt/Desktop/jens/untappd/")
library(ggplot2)
library(ggmap)
library(akima)
library(fields)
library(spatialfil)
library(plotly)
library(sp)
library(viridis)
rm(list = ls())
#
# Some Copenhagen tests
#
df <- data.frame(
  lon = rnorm(50, 12.56, .02), 
  lat = rnorm(50, 55.68, .005),
  val = rnorm(50)
)

# cph <- get_map(location = c(lon = 12.56, lat = 55.68),
#                color = "color",
#                #source = "google",
#                #maptype = "roadmap",
#                zoom = 13)

gg <- ggmap(cph,
            extent = "device", # "panel"
            ylab = "Latitude",
            xlab = "Longitude")
gg
#
temp <- interp(df$lon, df$lat, df$val)
dfExp <- expand.grid(x = temp$x, y = temp$y)
dfExp$val <- as.vector(temp$z)
dfExp <- na.omit(dfExp)
#
gg2 <- gg +
  stat_contour(data = dfExp, binwidth = 0.5, bins = 10,
               aes(x=x,y=y,z=val, fill = ..level..), geom = "polygon", alpha = 0.3) +
  geom_point(data = df, alpha = 0.5,
             aes(x = lon, y = lat))
gg2
# Lets fix the weird contours
BINS<-15
BINWIDTH<-(diff(range(dfExp$val))/BINS) # reference from ggplot2 code
arbitaryValue=min(dfExp$val)-BINWIDTH*1.5
#
# check x and y grid
minValue<-sapply(dfExp,min)
maxValue<-sapply(dfExp,max)
#
test1<-data.frame(x=minValue[1]-0.01,y=minValue[2]:maxValue[2], val=arbitaryValue)
test2<-data.frame(x=minValue[1]:maxValue[1],y=minValue[2]-0.01, val=arbitaryValue)
test3<-data.frame(x=maxValue[1]+0.01,y=minValue[2]:maxValue[2], val=arbitaryValue)
test4<-data.frame(x=minValue[1]:maxValue[1],y=maxValue[2]+0.01, val=arbitaryValue)
test<-rbind(test1,test2,test3,test4)
combi <- rbind(dfExp,test)
#
gg3 <- gg +
  stat_contour(data = combi, bins = BINS,
               aes(x=x,y=y,z=val, fill = ..level..), geom = "polygon", alpha = 0.3) +
  geom_point(data = df, alpha = 0.5,
             aes(x = lon, y = lat))
gg3 # Did not work.
#
# Test with real data
# 
rm(list = ls())
cph = readRDS("maps/cph.rds")
score = readRDS("testMapDat.rds")
#
gg <- ggmap(cph,
            extent = "device", # "panel"
            ylab = "Latitude",
            xlab = "Longitude")
gg
#
score = na.omit(score)
names(score)[names(score) == "lng"] = "lon"
score$val = score$Garbacz + score$hellegskov - score$Slendrick - score$knoe1703
#
lims = attr(cph,"bb")
score = subset(score, score$lat>lims$ll.lat & score$lat<lims$ur.lat & score$lon>lims$ll.lon & score$lon<lims$ur.lon)
mint = score
#
epsi = 0.001
N = dim(score)[1]
score[N+1,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ll.lat+epsi,0)
score[N+2,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ur.lat-epsi,0)
score[N+3,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ll.lat+epsi,0)
score[N+4,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ur.lat-epsi,0)
#
lonLim = seq(lims$ll.lon + epsi, lims$ur.lon - epsi, length.out = 40)
latLim = seq(lims$ll.lat + epsi, lims$ur.lat - epsi, length.out = 40)
#
# temp <- interp(score$lon, score$lat, score$val)
temp <- interp(x = score$lon, y = score$lat, z = score$val, xo = lonLim, yo = latLim, extrap = T, linear = F)
temp <- interp(x = score$lon, y = score$lat, z = score$val, linear = F, extrap = T)
temp <- interp.old(x = score$lon, y = score$lat, z = score$val, xo = lonLim, yo = latLim, extrap = T, ncp = 2)
scoreExp <- expand.grid(x = temp$x, y = temp$y)
scoreExp$val <- as.vector(temp$z)
scoreExp <- na.omit(scoreExp)
#

mint2 = scoreExp
scoreExp$val[scoreExp$val< -26] = -26
scoreExp$val[scoreExp$val> 26] = 26
gg2 <- gg +
  stat_contour(data = scoreExp, #breaks = seq(from =-26, to = 26, by = 6), #breaks = c(-26,-20,-14,-8,-2,4,10,16,22), #, #bins = 25, #binwidth = 0.005,# bins = 15,
               aes(x=x,y=y,z=val, fill = factor(..level..)), geom = "polygon", alpha = 1) + 
  #scale_fill_gradient(low="yellow", high="blue") +
  #scale_fill_gradient(low="#bcbd22", high="#0080FF") +
  scale_fill_viridis(discrete = T) +
  geom_point(data = score,
             aes(x = lon, y = lat, size = abs(val), color = sign(val))) 
gg2
#
gg4 <- gg + 
  #geom_density2d(data = scoreExp, aes(x = x, y = y)) + 
  geom_point(data = scoreExp, aes(x=x, y=y))
gg4

#
test = smooth.2d(Y = score$val, x = score[,c("lon","lat")], theta = 0.005, nrow = 32, ncol = 32)
scoreExp <- expand.grid(x = test$x, y = test$y)
scoreExp$val <- as.vector(test$z)
scoreExp <- na.omit(scoreExp)
hist(scoreExp$val)
ggplot(data = scoreExp) + stat_contour(aes(x = x, y = y, z = val))
#
test2 = as.image(Z = score$val, x = score[,c("lon","lat")])
test3 = test2$z
test3[is.na(test3)] = 0
image(test3)
test4 = applyFilter(x = test3, kernel = convKernel(sigma = 2.5, k = 'LoG'))
# test4 = applyFilter(x = test3, kernel = convKernel(sigma = 1.5, k = 'gaussian'))
image(test4)
scoreExp <- expand.grid(x = test2$x, y = test2$y)
scoreExp$val <- as.vector(test4)
scoreExp <- na.omit(scoreExp)
#
# scoreExp$val[scoreExp$val>max(score$val)] = max(score$val)
# scoreExp$val[scoreExp$val<min(score$val)] = min(score$val)
#


test5 = applyFilter(x = test4, kernel = convKernel(sigma = 1,k = "sharpen"))
image(test5)
#
ggplot(data = scoreExp) + stat_contour(aes(x = x, y = y, z = val))
#
#
# 
# plot_ly(x = score$lon, y=score$lat, z=score$val, type = "contour", autocontour = T, 
#         contours = list(end = 30, size = 5, start=-30),
#         line = list(smoothing = 0.85))

#
plot_ly(x = scoreExp$x, y=scoreExp$y, z=scoreExp$val, type = "contour", autocontour = F,
        #contours = list(end = 26, size = 6, start=-26),
        line = list(smoothing = 1.3))

scoreExp2 = scoreExp[order(scoreExp$x,scoreExp$y),]
dens <- contourLines(x = test2$x, y = test2$y, z = test4, levels = seq(from=-26,to = 26, by = 6))
for (i in 1:length(dens)) {
  tmp <- point.in.polygon(scoreExp$lon, scoreExp$lat, dens[[i]]$x, dens[[i]]$y)
  if (length(tmp)>0){
    scoreExp$Density[which(tmp==1)] <- mean(scoreExp$val[which(tmp == 1)])
  }
}
#
# Another test
#
rm(list = ls())
cph = readRDS("maps/cph.rds")
score = readRDS("testMapDat.rds")
#
gg <- ggmap(cph,
            extent = "device", # "panel"
            ylab = "Latitude",
            xlab = "Longitude")
gg
#
score = na.omit(score)
names(score)[names(score) == "lng"] = "lon"
score$val = score$Garbacz + score$hellegskov - score$Slendrick - score$knoe1703
#
lims = attr(cph,"bb")
score = subset(score, score$lat>lims$ll.lat & score$lat<lims$ur.lat & score$lon>lims$ll.lon & score$lon<lims$ur.lon)
mint = score
#
epsi = 0.001
N = dim(score)[1]
score[N+1,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ll.lat+epsi,0)
score[N+2,c("lon","lat","val")] <- c(lims$ll.lon+epsi,lims$ur.lat-epsi,0)
score[N+3,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ll.lat+epsi,0)
score[N+4,c("lon","lat","val")] <- c(lims$ur.lon-epsi,lims$ur.lat-epsi,0)
#
test2 = as.image(Z = score$val, x = score[,c("lon","lat")])
test3 = test2$z
test3[is.na(test3)] = 0
image(test3)
# test4 = applyFilter(x = test3, kernel = convKernel(sigma = 2.5, k = 'LoG'))
test4 = applyFilter(x = test3, kernel = convKernel(sigma = 4.5, k = 'gaussian'))
image(test4)
scoreExp <- expand.grid(x = test2$x, y = test2$y)
scoreExp$val <- as.vector(test4)
scoreExp <- na.omit(scoreExp)
#

#
plot_ly(x = scoreExp$x, y=scoreExp$y, z=scoreExp$val, type = "contour", autocontour = F,
        #contours = list(end = 20, size = 0.5, start=-20),
        line = list(smoothing = 1.3))

BREAKS = c(seq(from = 0.5*min(scoreExp$val), to = 0.5*max(scoreExp$val), length.out = 8),-0.01,0.01)
gg2 <- gg +
  stat_contour(data = scoreExp, breaks = BREAKS, #breaks = c(-26,-20,-14,-8,-2,4,10,16,22), #, #bins = 25, #binwidth = 0.005,# bins = 15,
               aes(x=x,y=y,z=val, fill = factor(..level..)), geom = "polygon", alpha = 0.5) + 
  #scale_fill_gradient(low="yellow", high="blue") +
  #scale_fill_gradient(low="#bcbd22", high="#0080FF") +
  scale_fill_viridis(discrete = T) +
  geom_point(data = score,
             aes(x = lon, y = lat, size = abs(val), color = sign(val))) 
gg2
#
dens <- contourLines(x = test2$x, y = test2$y, z = test4, levels = seq(from=-26,to = 26, by = 6))
scoreExp$dens = NA
for (i in 1:length(dens)) {
  inds <- point.in.polygon(scoreExp$x, scoreExp$y, dens[[i]]$x, dens[[i]]$y)
  indsMint <- point.in.polygon(score$lon, score$lat, dens[[i]]$x, dens[[i]]$y)
  if (length(indsMint)>0){
    # scoreExp$dens[which(inds==1)] <- mean(scoreExp$val[which(tmp == 1)])
    scoreExp$dens[which(inds==1)] <- mean(score$val[which(indsMint == 1)])
  }
}
scoreExp$dens[is.na(scoreExp$dens)] = 0
#
gg3 <- gg +
  stat_contour(data = scoreExp, breaks = seq(from =-26, to = 26, by = 6), #breaks = c(-26,-20,-14,-8,-2,4,10,16,22), #, #bins = 25, #binwidth = 0.005,# bins = 15,
               aes(x=x,y=y,z=val, fill = dens), geom = "polygon", alpha = 1) + 
  #scale_fill_gradient(low="yellow", high="blue") +
  #scale_fill_gradient(low="#bcbd22", high="#0080FF") +
  scale_fill_viridis(discrete = F) +
  geom_point(data = score,
             aes(x = lon, y = lat, size = abs(val), color = sign(val))) 
gg3
#
#
#
#
N = 128
x = seq(1,N)
y = seq(1,N)
g = expand.grid(x = x, y = y)
test = c(g$x,g$y)
xMat = matrix(data = test, nrow = N, ncol = N)
yMat = t(xMat)
#
y0 =  pi/3
x0 =  pi/3
#
S = -17
rMat = sqrt( 0*(xMat - x0[1])^2 + (yMat - y0[1])^2) 
image(rMat)
lambda = 0.1
scoreMat = S*exp(-lambda*rMat)
image(scoreMat)

lims = attr(cph,"bb")



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
