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
      menuItem("Map", tabName = "map"),
      menuItem("Users stats", tabName = "uStats"),
      menuItem("Team stats", tabName = "tStats"),
      menuItem("Venue stats", tabName = "vStats")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem("welcome",
              fluidRow(
                h2("This page is for a general introduction"),
                box(title = "Rules", 
                    h2("General rules"))
              )
      ),
      tabItem("map",
              fluidRow(column(4,
                              actionButton(inputId = "calcMap", label = "Calculate map")
                              ),
                       column(4,
                              numericInput(inputId = "mapAlpha", label = "Alpha value for the map", 
                                           value = 0.3, min = 0, max = 1, step = 0.1)
                              )
              ),
              tags$br(),
              fluidRow(
                plotOutput("beerMap", height = "600px")
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
      )
    )
  )
)
