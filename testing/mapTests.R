#
# 06-09-2018
#
# Map tests
.libPaths("c:/R_libs")
library(ggplot2)
library(ggmap)
# Example from https://www.r-bloggers.com/google-maps-and-ggmap/
mapImageData3 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 16)

ggmap(mapImageData3,
      extent = "device", # "panel"
      ylab = "Latitude",
      xlab = "Longitude")

#
# Example from  https://stackoverflow.com/questions/51197980/ggplot2-version-3-incompatibility-with-ggmap-for-geom-density-2d
#
# Create a data frame
df <- data.frame(
  long = rnorm(50, -122.32, .2), 
  lat = rnorm(50, 47.6, .2) 
)

# Use qmplot to create a base layer of map tiles
base_plot <- qmplot(
  data = df, 
  x = long, # data feature for longitude
  y = lat, # data feature for latitude
  geom = "blank", # don't display data points (yet)
  maptype = "terrain", # map tiles to query
  #darken = .7, # darken the map tiles
  legend = "topleft" # location of legend on page
)

# Show the map in RStudio
base_plot

# Use ggplot to create a 2d density map (without tiles -- works fine)
ggplot(df, aes(x = long, y = lat)) + 
  geom_density2d() + 
  stat_density_2d(
    aes(x = long, y = lat, fill = stat(level)), # in v2, fill = ..level..
    # Use the computed density to set the fill
    alpha = .3,
    geom="polygon" # Set the alpha (transparency)
  )

base_plot + 
  geom_density2d() + 
  stat_density_2d(
    aes(x = long, y = lat, fill = stat(level)), # in v2, fill = ..level..
    # Use the computed density to set the fill
    alpha = .3,
    geom="polygon" # Set the alpha (transparency)
  )
#
# Some Copenhagen tests
#
df <- data.frame(
  lon = rnorm(50, 12.56, .00002), 
  lat = rnorm(50, 55.68, .00002) 
)

mapTest <- get_map(location = c(lon = 12.56, lat = 55.68),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 13)
gg <- ggmap(mapTest,
            extent = "device", # "panel"
            ylab = "Latitude",
            xlab = "Longitude")
gg
gg2 <- gg + geom_density2d(data = df)
gg2

  geom_density2d(data = df) + 
  stat_density_2d(
    aes(x = lon, y = lat, fill = stat(level)), # in v2, fill = ..level..
    # Use the computed density to set the fill
    alpha = .3,
    geom="polygon" # Set the alpha (transparency)
  )
gg2
#
# geom_denisty2d test
#
df <- data.frame(
  x = rnorm(50, 10, .5), 
  y = rnorm(50, 10, .5),
  val = rnorm(50,1,0.5)
)
mint = df
ind = sample.int(50,10)
df2 = rbind(df,df[ind,])
df3 = rbind(df,
            df[1,],df[1,],df[1,],df[1,],df[1,],df[1,],df[1,],df[1,],
            df[2,],df[2,],df[2,],df[2,],df[2,],df[2,],df[2,],df[2,])
#
p <- ggplot(df) + geom_point(aes(x=x,y=y))
p
p2 <- p + geom_density2d(aes(x=x,y=y)) +
  scale_x_continuous(limits = c(8,12)) + 
  scale_y_continuous(limits = c(8,12))
p2
#
p3 <- p + #geom_density2d(aes(x=x,y=y)) + 
  stat_density_2d(data = df3, aes(x=x,y=y, fill = stat(level)), alpha = .3, geom="polygon") +
  scale_x_continuous(limits = c(8.5,12)) + 
  scale_y_continuous(limits = c(8.5,12))
p3

p4 <- ggplot(df, aes(x=x,y=y)) + geom_density2d() + stat_density_2d(aes( fill = stat(level)), alpha = .3, geom="polygon" )
p4
#
# Raster to polygon tests
#
library(raster)
library(sf)
library(units)
library(smoothr)
rm(list = ls())
# let see that plot
plot(rasterToPolygons(jagged_raster), col = NA, border = NA) # set up plot extent
plot(jagged_raster, col = heat.colors(100), legend = FALSE, add = TRUE)
# lets begin
r <- cut(jagged_raster, breaks = c(-Inf, 0.5, Inf)) - 1
plot(rasterToPolygons(r), col = NA, border = NA) # set up plot extent
plot(r, col = c("white", "#4DAF4A"), legend = FALSE, add = TRUE, box = FALSE)
# polygonize
r_poly <- rasterToPolygons(r, function(x){x == 1}, dissolve = TRUE) %>% 
  st_as_sf()
plot(r_poly, col = NA, border = "grey20", lwd = 1.5, add = TRUE)
r_poly_smooth <- smooth(r_poly, method = "ksmooth")
plot(r_poly_smooth, col = "#4DAF4A", border = "grey20", lwd = 1.5, add = TRUE)
#
#
# 
df <- data.frame(
  x = round(10*rnorm(50, 10, .5))/10, 
  y = round(10*rnorm(50, 10, .5))/10,
  val = rnorm(50,10,0.5)
)
p <- ggplot(df) + geom_tile(aes(x=x,y=y,fill = val))
p
#
# heat map test
#
mapTest <- get_map(location = c(lon = 12.56, lat = 55.68),
                   color = "color",
                   source = "google",
                   maptype = "roadmap",
                   zoom = 13)
N = dim(mapTest)[1]
lat = attr(mapTest,"bb")[c(1,3)]
lon = attr(mapTest,"bb")[c(2,4)]
#
df <- data.frame(
  lat = seq(from = lat[1,1], to = lat[1,2], length.out = N), 
  lon = seq(from = lon[1,1], to = lon[1,2], length.out = N),
  val = rnorm(n = N)
)
df = expand.grid(lat = seq(from = lat[1,1], to = lat[1,2], length.out = N),
                 lon = seq(from = lon[1,1], to = lon[1,2], length.out = N))
df$val = rnorm(n = N^2, mean = 5)
df$val[1:(N^2/2)]=0
#
p <- ggplot(df) + geom_tile(aes(x=lat,y=lon,fill = val))
p
gg <- ggmap(mapTest,
            extent = "device", # "panel"
            ylab = "Latitude",
            xlab = "Longitude")
gg
gg2 <- gg +  geom_tile(data = df, aes(x=lon, y=lat, fill = val), alpha = 0.3) # impossibly slow!
gg2
#
# Some raster test
#
mapTest <- get_map(location = c(lon = 12.56, lat = 55.68),
                   color = "color",
                   source = "google",
                   maptype = "roadmap",
                   zoom = 13)
N = dim(mapTest)[1]
lat = attr(mapTest,"bb")[c(1,3)]
lon = attr(mapTest,"bb")[c(2,4)]
#
df = expand.grid(lat = seq(from = lat[1,1], to = lat[1,2], length.out = N),
                 lon = seq(from = lon[1,1], to = lon[1,2], length.out = N))
df$val = rnorm(n = N^2, mean = 5)
df$val[1:(N^2/2)]=0
spg <- df
coordinates(spg) <- ~ lon + lat
gridded(spg) <- T
rasterDF <- raster(spg)

rtp <- rasterToPolygons(rasterDF)
