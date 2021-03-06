#
# 14-09-2018
# 
# Drinkioso v0.1
#
# setwd("/home/shiny/drinkioso")

source("setup.R")
library(shinydashboard)
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
                                                 choices = getTeam(opts = "name"),
                                                 selected = getTeam(opts = "name")
                                                 )
                              ),
                       column(4,
                              numericInput(inputId = "mapAlpha", label = "Alpha value for the map",
                                           value = 0.2, min = 0, max = 1, step = 0.1)
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
              ),
              br(),
              fluidRow(
                box(title = "Easy pickings! These venues have no owner or only a very small lead:", width = 10,
                 div(style = 'overflow-x: scroll',
                     DT::dataTableOutput(outputId = "easyPick")
                     )
                )
              )
      ),
      tabItem("uStats",
              fluidRow(
                column(3,
                       uiOutput("userChoice")
                       ),
                column(9,
                       DT::dataTableOutput(outputId = "userStats")
                       )
              ),
              br(),
              fluidRow(
                box(title = "Trophies", width = 10, status = "primary", solidHeader = T, collapsible = T, collapsed = T, # height = 150,
                    div(title = "Your well deserved trophies",
                        # style = 'overflow-y: scroll',
                        style = 'overflow:hidden',
                        h3("Some trophies"),
                        # fluidRow(
                        #   column(12,
                                 img(src = "imagetest.gif")
                        #          )
                        # )
                        )
                    )
              ),
              br(),
              fluidRow(column(3,
                              uiOutput("userPlotChoice")
                              ),
                       column(9,
                              plotOutput("userPlot")
                              )
                       )
      ),
      tabItem("tStats",
              fluidRow(
                column(4,
                       uiOutput("teamPlotChoice"),
                       uiOutput("teamMembers")
                ),
                column(8,
                       plotOutput("teamPlot")
                )
              ),
              fluidRow(
                column(12,
                       DT::dataTableOutput(outputId = "teamStats")
                       )
              )
      ),
      tabItem("vStats",
              fluidRow(
                column(12,
                       fluidRow(
                         column(3,
                                uiOutput(outputId = "venueChoice")
                         ),
                         column(9,
                                plotOutput(outputId = "venueTimePlot")
                                )
                       ),
                       br(),
                       fluidRow(
                         column(12,
                                DT::dataTableOutput(outputId = "venueStats")
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
                                   fluidRow(
                                     box(title = "User related", width = 12,
                                         fluidRow(
                                           column(4,
                                                  uiOutput("userChoiceAdmin"),
                                                  br()
                                           ),
                                           column(3,
                                                  actionButton(inputId = "updateUsers", label = "Update user data")
                                           ),
                                           column(2,
                                                  uiOutput("userTeam"),
                                                  uiOutput("userJoin")
                                                  ),
                                           column(2,
                                                  actionButton(inputId = "updateUserTeam", label = "Update user team"),
                                                  br(),
                                                  br(),
                                                  actionButton(inputId = "updateUserJoin", label = "Update user join date")
                                                  )
                                         ),
                                         fluidRow(
                                           column(12,
                                                  div(style = 'overflow-x: scroll',
                                                      DT::dataTableOutput(outputId = "userHistTab")
                                                  )
                                                  )
                                         )
                                     ),
                                     box(title = "Venue related", width = 10),
                                     box(title = "Scoring related", width = 10,
                                         column(4,
                                                uiOutput("startDate")
                                                )
                                         )
                                   )
                  )
                )
              )
    )
  )
)
