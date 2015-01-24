library(shiny)
# include my library of data functions
source("osp.R")

countries = c(
  "Cyprus"="315",
  "Ukraine"="301",
  "India" = "285",
  "Russia" = "275",
  "All" = "0")

platforms = c("Android" = "6", "Java" = "3", "All" = "0")

platforms_oms = c("Android" = "13", "Java" = "15", "All" = "0")


countries_downloads = c(
  "Cyprus"="315",
  "Ukraine"="301",
  "India" = "285",
  "Russia" = "275",
  "Indonesia" = "289",
  "Brazil" = "293",
  "All" = "-1")
 
# talk to osp database by default
db <<- "osp"

# define a global list
temp=c()

shinyServer(function(input, output) {
   
  output$plotPositions <- renderPlot({
    res = run_sql ("positions_analysis", input$subsite_positions, input$platform_positions)
    mydata <- res[[1]]
    temp <<- mydata
    title <- res[[2]]    
    pie(mydata[,2],labels=mydata[,1],col=rainbow(13))
    country <- names(countries)[match(input$subsite_positions, countries)]
    platform <- names(platforms)[match(input$platform_positions, platforms)]
    
    text(0, -1, paste0("Positions Share - ", country, " - ", platform), col = "black")  
  })
  
  output$outputPositions <- renderText({
    country <- names(countries)[match(input$subsite_positions, countries)]
    platform <- names(platforms)[match(input$platform_positions, platforms)]
   })
  
  output$plotCohorts <- renderPlot({
    res = run_sql ("cohort_analysis", input$subsite_cohorts, input$platform_cohorts)
    mydata <- res[[1]]
    temp <<- mydata
    title <- res[[2]]    
    mydata = as.matrix(mydata)  
    c<-cbind(mydata[,2],as.numeric(mydata[,3]))
    
    nam = c[,1]
    barplot(as.numeric(c[,2]), main=title, ylab= "Total", beside=TRUE, col=rainbow(5), names.arg=nam)

    country <- names(countries)[match(input$subsite_cohorts, countries)]
    platform <- names(platforms)[match(input$platform_cohorts, platforms)]
    
    text(0, -1, paste0("Cohorts - ", country, " - ", platform), col = "black")  
  })
  
  output$outputCohorts <- renderText({
    country <- names(countries)[match(input$subsite_cohorts, countries)]
    platform <- names(platforms)[match(input$platform_cohorts, platforms)]
 
  })
  
 
  output$plotMarketing <- renderPlot({
    res = run_sql ("channels_share", input$subsite_marketing, input$platform_marketing)
    mydata <- res[[1]]
    temp <<- mydata
    title <- res[[2]]    
     
    series = as.numeric(mydata[,1])
    labels = mydata[,2]
    country <- names(countries)[match(input$subsite_marketing, countries)]
    platform <- names(platforms)[match(input$platform_applisting, platforms)]

    pie(series, labels=labels, col=rainbow(4))
    text(0, -1, paste0("Marketing channels share - ", country, " - ", platform), col = "black")

  })
  
  output$outputMarketing <- renderUI({
    country <- names(countries)[match(input$subsite_applisting, countries)]
    platform <- names(platforms)[match(input$platform_applisting, platforms)]
    helpText("This is marketing analysis",br(),h4("Title"))
    
  })
  
  
  output$outputInstalls <- renderPlot ({ 
    country <- input$subsite_kpi_downloadsm

    platform <- input$platform_kpi_downloadsm

     kpi_monthly_downloads  (country,platform,TRUE,
                               input$monthlydownloadsDisplayNumber[1]:input$monthlydownloadsDisplayNumber[2])
    
 
  })
  
  output$help <- renderUI ({
 
                  includeMarkdown('help.Rmd')          
          
  })
  
})

