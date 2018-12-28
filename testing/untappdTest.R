# 28-08-2018
#
# Some untappd tests.
#
.libPaths("c:/R_libs")
library(jsonlite)
#
rm(list = ls())
#
id = "562203AF6B431C6653E6E7346ACB17FCB6AC5F14"
secret = "54E07A46395CE64ADB485888EEAE297D1B9D17FF"
link = "https://api.untappd.com/v4/"
# venue test
method = paste0("venue/info/", 1222)
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
venue = fromJSON(final)
print(venue$meta$code)

#### Brus example
# foursquar venue look up
fs_id = "56e2ad56498e053777005750" # Brus
method = "venue/foursquare_lookup/56e2ad56498e053777005750"
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
fLook = fromJSON(final)
ut_id = fLook$response$venue$items$venue_id
# venue info
method = paste0("venue/info/", ut_id)
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
venue = fromJSON(final)
print(venue$meta$code)

#### user examples
method = "user/info/JayTea"

method = "user/info/Garbacz"
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
ivan = fromJSON(final)

method = "user/info/Slendrick"
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
jeppe = fromJSON(final)

method = "user/beers/Slendrick"
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
beerJeppe = fromJSON(final)
# no location :(
method = "user/beers/Slendrick?limit=25"
method = "user/beers/Slendrick"
final = paste0(link,method,"?client_id=",id,"&client_secret=",secret)
temp = fromJSON(final)
temp = GET("Slendrick",path = "https://api.untappd.com/v4/user/beers/")
