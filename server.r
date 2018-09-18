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
                 })
  })
  
  output$mapHoverOut <- renderUI({
    mapHoverFunc(input$mapHover, data = mapList$score)
  })
  
  
  output$beerMap <- renderPlot({
    beerMap$map
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
}