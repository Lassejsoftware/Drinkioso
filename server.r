#
# 14-09-2018
#
# server side of drinkioso v0.1 
#
function(input, output, session) {
  beerReact <- reactiveValues()
  #### Welcome page ####
  
  #### map ####
  observeEvent(input$calcMap,{
    req(input$mapLambda)
    withProgress(message = "Calculating map",
                 {
                   score = makedfScore()
                   beerList <- createMap(score = score,
                                         map = cph, 
                                         lambda = input$mapLambda
                                         )
                   beerReact$mapList = beerList
                 })
  })
  
  # Make the plot
  output$beerMap <- renderLeaflet({
    req(beerReact$mapList)
    beerList <- beerReact$mapList
    BREAKS = c(seq(-40,40,by=10),-5,5,1,1,-0.1,0.1)
    CL <- contourLines(unique(beerList$scoreExp$x) , unique(beerList$scoreExp$y) , beerList$mapImage, levels = BREAKS)
    
    ## EXTRACT CONTOUR LINE LEVELS
    LEVS <- as.factor(sapply(CL, `[[`, "level"))
    NLEV <- length(levels(LEVS))
    
    ## CONVERT CONTOUR LINES TO POLYGONS
    pgons <- lapply(1:length(CL), function(i)
      Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
    spgons = SpatialPolygons(pgons)
    
    # With bars sommer::jet.colors
    leaflet(spgons) %>% addTiles() %>%
      addPolygons(color = sommer::jet.colors(NLEV, NULL)[LEVS], fillOpacity = input$mapAlpha) %>%
      # addPolygons(color = heat.colors(NLEV, NULL)[LEVS], fillOpacity = input$mapAlpha) %>%
      addCircles(lng = beerList$score$lon, lat = beerList$score$lat,
                 radius = abs(beerList$score$val), opacity = 1, col = beerList$score$col, fillOpacity = 1, label = lapply(beerList$score$label,HTML),
                 labelOptions = list(textsize = "15px")) 
  })
  
  #### user stats ####
  output$userChoice <- renderUI({
    users = dir("checkinHist")
    users = gsub(pattern = "\\..*", replacement = "", x = users)
    selectizeInput(inputId = "users", label = "Choose a user",
                   choices = users)
  })
  
  # Update user data
  observeEvent(input$updateUsers,{
    print("Updating users.")
    print("Well not really.")
  })
  
  #### team stats ####
  
  #### venue stats ####
  
  output$venueChoice <- renderUI({
    venueNames <- makeVenueList(map = cph)
    selectizeInput(inputId = "venueNames", 
                   label = "Venues",
                   choices = sort(venueNames$names))
  })
  
  #### Admin panel ####
  output$adminSwitch <- reactive({
    if (input$adminPass == "storepatter"){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, 'adminSwitch' , suspendWhenHidden=FALSE)
}