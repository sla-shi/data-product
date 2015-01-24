library(shiny)
 
countries = c(
              "Cyprus"="315",
              "Ukraine"="301",
              "India" = "285",
              "Russia" = "275",
              "Uganda" = "797",
              "All" = "0")

platforms = c("Android" = "6",
              "Java" = "3",
              "All" = "0")

countries_positions   <- countries
platforms_positions   <- platforms
countries_cohort      <- countries
platforms_cohort      <- platforms
countries_applisting  <- countries
platforms_applisting  <- platforms

platforms_oms = c("Android" = "13", "Java" = "15", "Symbian" = "12", "Blackberry"="14", "All" = "0")
platforms_downloads = c("All" = "0")
countries_downloads = c("All" = "-1")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage("Product Statistics", header={hr()}, footer={hr();p("Product Statistics")}, fluid=TRUE,
             tabPanel("Help", 
                      mainPanel( 
                              uiOutput("help")
                      )
             ),
             tabPanel("Installs Monthly", 
                      sidebarLayout(
                              sidebarPanel(
                                      selectInput("subsite_kpi_downloadsm", "Select country:", countries_downloads),
                                      selectInput("platform_kpi_downloadsm", "Select platform:", platforms_downloads ),
                                      sliderInput("monthlydownloadsDisplayNumber", "How many months to display?", 
                                                  min=1, max=30, value=c(1,6), step=1)
                              ),
                              mainPanel( 
                                      plotOutput("outputInstalls") 
                              )
                      )
             ),
             tabPanel("Positions Analysis", 
                      sidebarLayout(
                            sidebarPanel(
                                selectInput("subsite_positions", "Select country:", countries_positions),
                                selectInput("platform_positions", "Select platform:", platforms_positions)
                             ),
                          mainPanel(
                            #title = "Positions Detailed View",
                            plotOutput('plotPositions'),
                            textOutput("outputPositions"),
                            hr()
                          )
                      )
                ),
             
             tabPanel("Cohort Analysis", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("subsite_cohorts", "Select country:", countries_cohort),
                          selectInput("platform_cohorts", "Select platform:", platforms_cohort)
                        ),
                        mainPanel(
                          plotOutput('plotCohorts'),
                          textOutput("outputCohorts"),
                          hr()
                        )
                      )
             ),

             
             tabPanel("Marketing Channels", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("subsite_marketing", "Select country:", countries),
                          selectInput("platform_marketing", "Select platform:", platforms)
                        ),
                        mainPanel(
                          plotOutput('plotMarketing'),
                          textOutput("outputMarketing"),
                          hr()
                        )
                      )
             )
             
            
  )
  ))