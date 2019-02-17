#
# 14-09-2018
#
# server side of drinkioso v0.1 
#

startDate = readRDS("extra/data/startDate.rds")

function(input, output, session) {
  # Reactives for different features.
  admin <- reactiveValues(start = startDate, #as.POSIXct("01 jan 2018", format = "%d %b %Y"),
                          score = makedfScore(
                            startDate = startDate,
                            map = cph),
                          calc = 1,
                          map = cph
  )
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
  observeEvent(c(input$calcMap, admin$calc),{
    req(input$mapLambda)
    withProgress(message = "Calculating map", value = 1,
                 {
                   beerList <- createMap(score = admin$score,
                                         lambda = input$mapLambda
                                         )
                   beerReact$mapList = beerList
                 })
  })
  
  # Make the plot
  output$beerMap <- renderLeaflet({
    print(admin$start)
    req(beerReact$mapList)
    beerList <- beerReact$mapList
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
    
    # Get playing field limits
    # lims = makedfVenue(map = admin$map, opts = "lims")
    
    # With bars sommer::jet.colors
    const = 10
    leaflet(spgons) %>% addTiles() %>%
      addPolygons(color = matlab::jet.colors(NLEV)[LEVS], fillOpacity = fillOVec, opacity = oVec) %>%
      addCircles(lng = beerList$score$lon, lat = beerList$score$lat,
                 radius = abs(beerList$score$val)+const, opacity = 1, col = beerList$score$col, fillOpacity = 1, label = lapply(beerList$score$label,HTML),
                 labelOptions = list(textsize = "15px")) %>% 
      addCircles(lng = beerList$score$lon, lat = beerList$score$lat,
                 radius = abs(beerList$score$val) + const, opacity = 1, col = "black", fill = F, weight = 3) 
      # addRectangles(lng1 = lims$ll.lon, lat1 = lims$ll.lat, lng2 = lims$ur.lon, lat2 = lims$ur.lat,
      #               fillOpacity = 0, color = "#000000", dashArray = "3")
    
  })
  
  #### user stats ####
  output$userChoice <- renderUI({
    users = getUsers(team = T)
    selectizeInput(inputId = "user", label = "Choose a user",
                   choices = users)
  })
  
  output$userStats <- DT::renderDataTable({
    req(input$user)
    userInfo = readRDS( paste0("../drinkiosoData/users/",input$user,".rds") )
    if (is.null(userInfo$joinDate)){joinDate = admin$start} else {joinDate = userInfo$joinDate}
    joinDate = gsub("\\ .*", "",joinDate)
    dfOut = data.frame(Team = proper(getTeam(user = input$user)) , 
                       "Join date" = joinDate,
                       "Total beers had" = sum(admin$score[[input$user]]),
                       "Mi casa" = admin$score$venue_name[which.max(admin$score[[input$user]])],
                      check.names = F)
    
    datatable(dfOut, rownames = F, options = list(dom = "t")) %>% formatStyle(
      "Team", 
      #target = 'row',
      backgroundColor = styleEqual(c("Blue", "Red"), c("#619CFF", "#F8766D")) # c("lightblue", "#ff3232")
    )
    
  })
  
  output$userPlotChoice <- renderUI({
    selectizeInput(inputId = "userPlotChoice", label = "Choose a plot",
                   choices = userPlotWrapper(opts = "plots")
                   )
  })
  
  output$userPlot <- renderCachedPlot({
    req(input$user, input$userPlotChoice)
    userPlotWrapper(user = input$user, plotType = input$userPlotChoice, startDate = admin$start, map = cph)
  }, cacheKeyExpr = list(input$user, input$userPlotChoice, admin$start))
  
  # 
  # output$testImage <- renderImage({
  #   list(src = "imagetest.gif")
  # }, deleteFile = F)
  
  #### team stats ####
  
  output$teamPlotChoice <- renderUI({
    selectizeInput(inputId = "teamPlotChoice", label = "Choose a plot",
                   choices = teamPlotWrapper(opts = "plots")
    )
  })
  
  output$teamPlot <- renderCachedPlot({
    req(input$teamPlotChoice)
    teamPlotWrapper(plotType = input$teamPlotChoice, startDate = admin$start, score = admin$score, map = cph)
  }, cacheKeyExpr = list(input$teamPlotChoice, admin$start) )
  
  # Dropdown menu with list of members. NOT READY FOR MORE TEAMS
  output$teamMembers <- renderUI({
    users = getUsers(team = T)
    teams = getTeam(user = users)
    
    
    dropdownButton(status = "default", circle = F, label = "Team members",
      {
        fluidRow(
          # Red
          column(6,
                 HTML(paste0("<b>Red</b> <br> <br>",  paste0(users[teams == "red"], collapse = "<br>") ) )
          ),
          # Blue
          column(6,
                 HTML(paste0("<b>Blue</b> <br> <br>",  paste0(users[teams == "blue"], collapse = "<br>") ) )
          )
        )
    })
  })
  
  # Table with some stats
  output$teamStats <-  DT::renderDataTable({
    cats = c("Team Hideout",
             "Most valuable drinker",
             "Number of owned bars"
    )
    dfTeam = data.frame("Category" = cats,
                        Red = "empty",
                        Blue = "empty",
                        check.names = F,
                        row.names = cats,
                        stringsAsFactors = F)
    score = admin$score
    # Highest scored venue
    dfTeam[cats[1],proper(getTeam(val = 1 ))] = as.character(score$venue_name[which.max(score$val)])
    dfTeam[cats[1],proper(getTeam(val = -1))] = as.character(score$venue_name[which.min(score$val)])
    
    # Most valuable drinker
    users = getUsers(team = T)
    dfRes = data.frame(user = rep("start", length(users)), team = rep("start", length(users)), val = rep(0, length(users)), stringsAsFactors = F)
    dfVenue = makedfVenue(map = admin$map)
    cc = 1
    for (i in users){ # Perhaps llaply this fucker?
      checks = readRDS(paste0("../drinkiosoData/checkinHist/",i,".rds"))
      checks = subset(checks, checks$venue_id %in% dfVenue$venue_id)
      info = readRDS(paste0("../drinkiosoData/users/",i,".rds"))
      checks$time = untappd2POSIXct(checks$created_at)
      if (!is.null(info$joinDate)){
        checks = subset(checks, checks$time > admin$start & checks$time > info$joinDate & !is.na(checks$venue_id) )
      } else {
        checks = subset(checks, checks$time > admin$start & !is.na(checks$venue_id))
      }
      dfRes$user[cc] = i
      dfRes$team[cc] = proper(getTeam(user = i))
      dfRes$val[cc] = nrow(checks)
      cc = cc + 1
    }
    dfAgg = dfRes %>% group_by(team) %>% filter(val == max(val))
    for (i in dfAgg$team){
      dfTeam[cats[2],i] = dfAgg$user[dfAgg$team == i]
    }
    
    # Number of bars
    counts = count_unique(score, "col")
    counts = subset(counts, counts$col != "black")
    counts$col = proper(counts$col)
    for (i in unique(counts$col)){
      dfTeam[cats[3], i] = counts$N[counts$col == i]
    }
    
    # Output table
    datatable(dfTeam, rownames = F, options = list(dom = "t"))%>% 
      formatStyle(
      "Red",
      backgroundColor = "#F8766D"
      ) %>%
      formatStyle(
        "Blue",
        backgroundColor = "#619CFF"
      )
  })
  
  
  #### venue stats ####
  
  output$easyPick <- DT::renderDataTable({
    req(beerReact$mapList$score)
    
    # score = beerReact$mapList$score
    score = admin$score
    dfOut = subset(score, abs(score$val)<2 & score$isBar == T, select = c("venue_name", "val","col") ) # ,users
    dfOut$col = proper(dfOut$col)
    dfOut$col[dfOut$val == 0] = "No owner"
    dfOut = dfOut[order(abs(dfOut$val)),]
    dfOut$val = abs(dfOut$val) + 1
    
    vec = c(val = "Beers needed for dominance", col = "Owner", venue_name = "Venue")
    dfOut = plyr::rename(dfOut,vec)
    datatable(dfOut, rownames = F) %>% formatStyle(
      "Owner", 
      #target = 'row',
      backgroundColor = styleEqual(c("Blue", "Red"), c("#619CFF", "#F8766D")) # c("lightblue", "#ff3232")
    )
  }) 
  
  output$venueChoice <- renderUI({
    selectizeInput(inputId = "venueChoice", 
                   label = "Venues",
                   choices = sort(trimws(as.character(admin$score$venue_name)))
                   )
  })
  
  observeEvent(input$venueChoice,{
    req(input$venueChoice)
    score = admin$score
    # Lets just assume venue_names are somewhat unique
    venueId = score$venue_id[trimws(score$venue_name) == input$venueChoice]
    
    # plot
    output$venueTimePlot = renderPlot({
      plotList <- venueTimePlot(venueId = venueId, startDate = admin$start)
      return(plotList$plot)
    })
  })
  
  output$venueStats <- DT::renderDataTable({
    score = admin$score
    users = getUsers(team = T)
    # dfVen = subset(score, score$venue_id == venueId, select = c("venue_name", "col", "val", users))
    dfVen = subset(score, !is.na(score$venue_id), select = c("venue_name", "col", "val", users) )
    dfVen$tot = rowSums(dfVen[,users])
    dfVen = subset(dfVen, !is.na(dfVen$col), select = c("venue_name", "col", "val", "tot", users) )
    dfVen$col = proper(dfVen$col)
    dfVen$col[dfVen$val == 0] = "No owner"
    dfVen$val = abs(dfVen$val)
    dfVen = arrange(dfVen, desc(val))
    # From -> to
    nameVec = c(venue_name = "Venue Name", col = "Owner", val = "Score", tot = "Total beers at venue")
    dfVen = plyr::rename(dfVen, nameVec)
    
    blueVec = which(getTeam(users) == "blue")
    redVec = which(getTeam(users) == "red")
    
    datatable(dfVen, rownames = F, options = list(pageLength = 50)) %>% formatStyle(
      users[blueVec],
      backgroundColor =  c("#619CFF") 
    ) %>% formatStyle(
      users[redVec],
      backgroundColor =  c("#F8766D")
    ) %>% formatStyle( 
      "Owner",
      backgroundColor = styleEqual(c("Blue", "Red"), c("#619CFF", "#F8766D"))
    )
  })
  
  #### Admin panel ####
  
  output$adminSwitch <- reactive({
    if (input$adminPass == "store patter"){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  outputOptions(output, 'adminSwitch' , suspendWhenHidden=FALSE)
  
  ## User related
  output$userChoiceAdmin <- renderUI({
    users <- getUsers()
    selectizeInput(inputId = "usersAdmin", label = "Choose a user to update or add new user",
                   choices = users,
                   multiple = F,
                   options = list(
                     create = T, # Possible to create new user
                     persist = T # Not possible to choose the new option as a usual option
                   ))
  })
  
  # User teams
  output$userTeam <- renderUI({
    req(input$usersAdmin)
    if (length(input$usersAdmin)== 1){
      radioButtons(inputId = "adminUserTeam", label = "Team",
                   choices = c(getTeam(opts = "name"), "delete"),
                   selected = proper(getTeam(user = input$usersAdmin))
                   )
    }
  })
  
  # User joinDate date
  output$userJoin<- renderUI({
    req(input$usersAdmin)
    users <- getUsers()
    if (input$usersAdmin %in% users){
      user = readRDS(paste0("../drinkiosoData/users/", input$usersAdmin, ".rds"))
      if (is.null(user$joinDate)){
        user$joinDate <- startDate
      }
      sliderInput(inputId = "adminUserJoin",
                  label = "Join date for user",
                  min = startDate,
                  max = as.POSIXct(Sys.Date()+5, format = "%Y-%b-%d"),
                  value = user$joinDate,# as.POSIXct(user$joinDate, format = "%d %b %Y"),
                  step = 14)
    }
  })
  
  # Users drinking history
  output$userHistTab <- DT::renderDataTable({
    req(input$usersAdmin)
    if (length(input$usersAdmin)== 1){
      users <- getUsers()
      if (input$usersAdmin %in% users){
        userDat = readRDS(paste0("../drinkiosoData/checkinHist/", input$usersAdmin, ".rds"))
        datatable(userDat, rownames = F, options = list(pageLength = 10))
      }
    }
  })
  
  # Update user data
  observeEvent(input$updateUsers,{
    req(input$usersAdmin)
    users = getUsers()
    updateUsers <- input$usersAdmin
    for (i in updateUsers){
      if (i %in% users){
        print("Updating user.")
        withProgress(message = "Getting user history", value = 1, {
          getUserHist(user = i, wTime = 60)
        })
      } else {
        print("Creating user.")
        showModal(modalDialog(
          fluidRow(h2(paste0("The user:", i," will be created")))
        )
        )
        withProgress(message = "Creating user in database", value = 1, {
          getUserHist(user = i, wTime = 60)
        })
      }
    }
    
    # Recalc user stats
    # trophies = trophyWrapper(opts = "trophies")
    # for (i in trophies){
    #   trophyWrapper(trophy = i, startDate = admin$start, map = admin$map, reCalc = T)
    # }
    
    # Delete cache
    file.remove(dir("myapp-cache/",full.names = T))
    
    # Recalc score
    admin$score = makedfScore(startDate = admin$start, map = admin$map)
  })
  
  # Update user team
  observeEvent(input$updateUserTeam, {
    req(input$usersAdmin)
    req(input$adminUserTeam)
    users = getUsers()
    if (input$usersAdmin %in% users){
      teamList <- readRDS("../drinkiosoData/teamList.rds")
      
      # Delete user from current team
      ind = grep(input$usersAdmin, teamList)
      if (length(ind)){
        temp = teamList[[ind]]
        teamList[[ind]] = temp[temp != input$usersAdmin]
      }
      
      # Update team
      if (input$adminUserTeam != "delete"){
        ind = which(tolower(input$adminUserTeam) == names(teamList))
        teamList[[ind]] <- c(teamList[[ind]], input$usersAdmin)
        saveRDS(teamList, "../drinkiosoData/teamList.rds")
        print(teamList)
      } else {
        saveRDS(teamList, "../drinkiosoData/teamList.rds")
        print(teamList)
      }
      admin$score <- makedfScore(startDate = startDate, map = cph)
    }
  })
  
  # Update user join date
  observeEvent(input$updateUserJoin, {
    req(input$usersAdmin)
    req(input$adminUserJoin)
    users <- getUsers()
    if (input$usersAdmin %in% users){
      user = readRDS(paste0("../drinkiosoData/users/", input$usersAdmin, ".rds"))
      if (is.null(user$joinDateBC)){
        if (is.null(user$joinDate)){
          user$joinDateBC <- startDate
        } else {
          user$joinDateBC <- user$joinDate
        }
      }
      user$joinDate <- input$adminUserJoin
      saveRDS(user, paste0("../drinkiosoData/users/", input$usersAdmin, ".rds"))
      admin$score <- makedfScore(startDate = startDate, map = cph)
    }
  })
  
  ## Scoring related
  output$startDate = renderUI({
    sliderInput(inputId = "startDate", 
                label = "Start date for Drinkioso",
                min = as.POSIXct("01 jan 2018", format = "%d %b %Y"),
                max = as.POSIXct(Sys.Date(), format = "%Y-%b-%d"), 
                value = as.POSIXct("01 jan 2018", format = "%d %b %Y"),
                step = 14)
  })
  
  observeEvent(input$startDate,{
    if (!is.null(input$startDate)){
      admin$start = input$startDate
      admin$score = makedfScore(startDate = admin$start, map = cph)
      admin$calc = admin$calc + 1
    }
  })
  
}