#
# 04-09-2018
# shinyapps.io setup.
# mail: jayearlgray
# pass: beerApp
# app-path: lassejsoftwarebeerapp
#
.libPaths("c:/R_libs")
# install.packages("rsconnect")
library(rsconnect)
#
rsconnect::setAccountInfo(name='lassejsoftware',
                          token='909F58ED1181BDED4806C8BC886EFB5B',
                          secret='jp0uhvD1ndsqgepxkgH79nmcEdkhl+jixR7x3k68')

rsconnect::deployApp('C:/Users/jqmt/Desktop/jens/LJSoftware/overView/')
#rsconnect::deployApp('C:\\Users\\jqmt\\Desktop\\jens\\LJSoftware\\overView\\')
deployApp()
