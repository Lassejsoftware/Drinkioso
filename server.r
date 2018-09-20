#
# 14-09-2018
#
# server side of drinkioso v0.1 
#
function(input, output, session) {
  beerMap <- reactiveValues()
  #### Welcome page ####
  
  #### map ####
  observeEvent(input$calcMap,{
    req(input$mapAlpha)
    withProgress(message = "Calculating map",
                 {
                   score = makedfScore()
                   mapList <<- createMap(score = score, map = cph, 
                                         alpha = input$mapAlpha, 
                                         sigma = input$mapSigma)
                   beerMap$map <- mapList$plot
                   beerMap$score <- score
                 })
  })
  
  # Make the plot
  output$beerMap <- renderPlot({
    beerMap$map
  })
  
  # Map hover
  output$mapHoverOut <- renderUI({
    mapHoverFunc(input$mapHover, data = mapList$score)
  })
  
  # Map clicker
  observeEvent(input$mapClick,{
    print("Map was clicked")
    points <- nearPoints(beerMap$score, input$mapClick, 
                         threshold = 10,
                         maxpoints = 1,
                         xvar = "lon",
                         yvar = "lat")
    if (dim(points)[1] == 1){
      print(points)
      updateTabItems(session, inputId = "menu", selected = "vStats")
      updateSelectizeInput(session, inputId = "venueNames",
                           selected = points$venue_name)
    }
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
  })
  
  #### team stats ####
  
  #### venue stats ####
  
  output$venueChoice <- renderUI({
    venueNames <- makeVenueList()
    selectizeInput(inputId = "venueNames", 
                   label = "Venues",
                   choices = venueNames$names)
  })
}