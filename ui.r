#
# 14-09-2018
# 
# Drinkioso v0.1
#
source("setup.R")
#
dashboardPage(
  #
  header = dashboardHeader(title = "Drinkioso"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome page", tabName = "welcome"),
      menuItem("Map", tabName = "map", selected = T),
      menuItem("Users stats", tabName = "uStats"),
      menuItem("Team stats", tabName = "tStats"),
      menuItem("Venue stats", tabName = "vStats"),
      menuItem("Admin panel", tabName = "admin"),
      id = "menu"
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem("welcome",
              fluidRow(
                div(title = "test",
                    imageOutput("logo")
                    )
                # box(title = "Rules",
                #   h2("General rules"))
              )
      ),
      tabItem("map",
              fluidRow(column(3,
                              actionButton(inputId = "calcMap", label = "Calculate map"),
                              br(),
                              checkboxGroupInput(inputId = "mapTeams", label = "Show teams on map",
                                                 choices = proper(names(getTeam(opts="teams"))),
                                                 selected = proper(names(getTeam(opts="teams")))) 
                              ),
                       column(4,
                              numericInput(inputId = "mapAlpha", label = "Alpha value for the map", 
                                           value = 0.3, min = 0, max = 1, step = 0.1)
                              ),
                       column(5,
                              numericInput(inputId = "mapLambda", label = "Decay constant for smoothing function",
                                           value = 650, min = 500, max = 700, step = 50)
                              )
              ),
              tags$br(),
              fluidRow(
                leafletOutput("beerMap",
                              height = 800)
              )
      ),
      tabItem("uStats",
              fluidRow(
                actionButton(inputId = "updateUsers", label = "Update user data")
              ),
              tags$br(),
              fluidRow(
                uiOutput("userChoice")
              )
      ),
      tabItem("tStats",
              fluidRow(
                h2("there will be stuff here")
              )     
      ),
      tabItem("vStats",
              fluidRow(
                column(12,
                       fluidRow(
                         tableOutput(outputId = "easyPick" )
                       ),
                       fluidRow(
                         column(4,
                                uiOutput(outputId = "venueChoice")
                         )
                       )
                       )
              )     
      ),
      tabItem("admin",
              fluidRow(
                column(3,
                       textInput(inputId = "adminPass", label = "Admin pass", placeholder = "Type password")
                       )
                ),
                tags$br(),
                fluidRow(
                  conditionalPanel("output.adminSwitch == true",
                    h4("Nothing here!")
                  )
                )
              )
    )
  )
)
