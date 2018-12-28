#
# adapted from https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package
#
#
.libPaths("c:/R_libs")
setwd("C:/Users/jqmt/Desktop/jens/drinkioso/")
#
library("leaflet")
#library("data.table")
library("sp")
#library("rgdal")
#library("KernSmooth")
library("tidyr")
library(htmltools)
#
inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
infile <- "testing/mvthefts.csv"
if(!file.exists(infile)){
  download.file(url = inurl, destfile = infile)
}
dat <- data.table::fread(infile)
setnames(dat, tolower(colnames(dat)))
setnames(dat, gsub(" ", "_", colnames(dat)))
dat <- dat[!is.na(longitude)]
dat[ , date := as.IDate(date, "%m/%d/%Y")]
#
kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
#
# Copenhagen attempt
#
source("scoring.R")
source("makedfVenue.R")
source("createMapV3.R")
cph = readRDS("maps/cph.rds")
score = makedfScore()
#
mapList = createMap(score, cph, lambda = 650)
#
score = mapList$score
score$col = "blue"
score$col[score$val>0] = "green"
scoreExp = mapList$scoreExp

# Make labels
users = dir("checkinHist/")
users = gsub("\\..*", "", users)
team = ifelse(score$val>0, "Green", "Blue")
inds = which(names(score) %in% users)
drinker = c()
num = c()
for (i in 1:dim(score)[1]){
  drinker[i] = names(which.max(score[i,inds]))
  num[i] = max(score[i,inds])
}
label = paste0("<b> Venue: </b>", score$venue_name, " owned by team <b>", team,  "</b> <br/>",
               "<b> Venue score is:  </b>", abs(score$val) , "<br/>",
               "<b> Best team player: </b>", drinker, " with ", num, " beers!"
)
score$label = label

BREAKS = c(seq(-40,40,by=10),-5,5,1,1,-0.1,0.1)
CL <- contourLines(unique(scoreExp$x) , unique(scoreExp$y) , mapList$mapImage, levels = BREAKS)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

# With bars
leaflet(spgons) %>% addTiles() %>%
  # addPolygons(color = heat.colors(NLEV, NULL)[LEVS], fillOpacity = 0.5) %>%
  addPolygons(color = sommer::jet.colors(NLEV, NULL)[LEVS], fillOpacity = 0.5) %>%
  addCircles(lng = score$lon, lat = score$lat,
             radius = abs(score$val), opacity = 1, col = score$col, fillOpacity = 1, label = lapply(score$label,HTML),
             labelOptions = list(textsize = "15px")) 

mymap_shape_click
mymap_click