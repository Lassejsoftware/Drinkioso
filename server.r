#
# 14-09-2018
#
# server side of drinkioso v0.1 
#
function(input, output, session) {
  beerReact <- reactiveValues()
  #### Welcome page ####
  
  output$logo <- renderImage({
    outfile = "www/drinkioso.png"
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         #width = 400,
         #height = 300,
         alt = "This is alternate text")
  }, deleteFile = F)
  
  #### map ####
  observeEvent(input$calcMap,{
    req(input$mapLambda)
    withProgress(message = "Calculating map", value = 1,
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
    tBeerList <<- beerList
    BREAKS = c(seq(-40, 40, by=10),-5,5,-1,1,-0.1,0.1) # 
    CL <- contourLines(unique(beerList$scoreExp$x) , unique(beerList$scoreExp$y) , beerList$mapImage, levels = BREAKS)
    levCL = sapply(CL, function(cs){cs$level})
    CL = CL[order(abs(levCL))]
    ## EXTRACT CONTOUR LINE LEVELS
    LEVS <- as.factor(sapply(CL, `[[`, "level"))
    NLEV <- length(levels(LEVS))
    
    fillOVec = input$mapAlpha*rep.int(1, length(LEVS))
    oVec = rep.int(1, length(LEVS))
    LEVS2 =  as.numeric(as.character(LEVS)) # Sometimes R!
    
    fillOVec[abs(LEVS2)<0.1]=0
    oVec[abs(LEVS2)<0.1]=0
    
    # WILL NOT WORK WITH MORE THAN 2 TEAMS!
    if (length(input$mapTeams) < length(names(getTeam(opts = "teams")))){
      team = input$mapTeams
      val = getTeam(col = team)
      if (val>0){
        fillOVec[LEVS2<0] = 0
        oVec[LEVS2<0] = 0
      } else {
        fillOVec[LEVS2>0] = 0
        oVec[LEVS2>0] = 0
      }
    } 
    
    ## CONVERT CONTOUR LINES TO POLYGONS
    pgons <- lapply(1:length(CL), function(i)
      Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
    spgons = SpatialPolygons(pgons)
    
    # With bars sommer::jet.colors
    leaflet(spgons) %>% addTiles() %>%
      addPolygons(color = sommer::jet.colors(NLEV, NULL)[LEVS], fillOpacity = fillOVec, opacity = oVec) %>%
      # addPolygons(color = heat.colors(NLEV, NULL)[LEVS], fillOpacity = input$mapAlpha) %>%
      addCircles(lng = beerList$score$lon, lat = beerList$score$lat,
                 radius = abs(beerList$score$val), opacity = 1, col = beerList$score$col, fillOpacity = 1, label = lapply(beerList$score$label,HTML),
                 labelOptions = list(textsize = "15px")) %>% 
      addCircles(lng = beerList$score$lon, lat = beerList$score$lat,
                 radius = abs(beerList$score$val), opacity = 1, col = "black", fill = F, weight = 3) 
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
  
  output$easyPick <- renderTable({
    req(beerReact)
    score = beerReact$mapList$score
    users = dir("checkinHist/")
    users = gsub("\\..*", "", users)
    users = users[users %in% names(score)]
    dfOut = subset(score, tolower(score$col) == "black" & score$isBar == T, select = c("venue_name", users))
    # FIX THIS SHIT!
    for (i in users){
      dfOut[[i]] = dfOut[[i]]*getTeam(i,opts="num")
    }
    return(dfOut)
  }, 
  caption = "<b> <span style='color:#000000'> Easy pickings! <br/> Nobody owns these bars: </b>",
  caption.placement = getOption("xtable.caption.placement", "top"))
  
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