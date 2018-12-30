#
# 04-09-2018
# shinyapps.io setup.
# mail: jayearlgray
# pass: beerApp
# app-path: lassejsoftwarebeerapp
#
.libPaths("c:/R_libs")
setwd("C:/Users/jqmt/Desktop/jens/drinkioso/")
# install.packages("rsconnect")
library(rsconnect)
#
rsconnect::setAccountInfo(name='lassejsoftware',
                          token='909F58ED1181BDED4806C8BC886EFB5B',
                          secret='jp0uhvD1ndsqgepxkgH79nmcEdkhl+jixR7x3k68')

# rsconnect::deployApp('C:/Users/jqmt/Desktop/jens/LJSoftware/overView/')
#rsconnect::deployApp('C:\\Users\\jqmt\\Desktop\\jens\\LJSoftware\\overView\\')
deployApp()

I read this very fine [post](https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package) and tried to make my own heatmap. However I have found that the solution does not accurately represent the heatmap data. Here is a small example of my problem. 
level is supposed to represent the height/z-value of that area. Since leaflet over plots, the small embedded square is not the same color as the right even though they have the same level.  c1,c2,c3 are lists which in practice could have come from contourLines().

c1 = list(level = 10, x = c(0,0,2,2), y = c(0,2,2,0))
c2 = list(level = -10, x = c(0,0,2,2) + 2, y = c(0,2,2,0))
c3 = list(level = -10, x = c(0,0,2,2)/2 + 1, y = c(0,2,2,0)/2 + 0.5)
CL = list(c1,c2,c3)
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

leaflet(spgons) %>% 
  addPolygons(color = sommer::jet.colors(NLEV, NULL)[LEVS], opacity = 0)

Can any one figure out a good solution which will generalize to a lot of irregular contours and which will respect the level attribute? 
  
  