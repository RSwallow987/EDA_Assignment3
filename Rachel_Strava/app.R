# a) allows the user to map their run i.e. to visualize their run on a map,
# b) provides the user with different summary statistics about their runs 
# and allows them to interrogate the dataset of their runs in different ways. 
# The app should be accompanied by documentation (in any format e.g. Rmd, docx, pdf) 
# explaining how to use the app and giving a short worked example demonstrating the app.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Load Librarys
library(shiny)
library(leaflet)
library(raster)
library(spData)
library(dplyr)
library(tidyverse)
library(leaftime)
library(lubridate)

#Load data
my_data <- readRDS("~/2021/EDA/EDA workspace/EDA_Assignment3/Rachel_Strava/data/my_data.rds")#Is this a probelm 
#x<-c(1,2,3,4,5)
# handling the data conversions 
my_data<- lapply(my_data, function(x) {
  x$date <- ymd(x$date)
  x$time<-hms(x$time)
  x #why this ?
})

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  titlePanel("My Strava App"),
  #PAGE 1 
  sidebarPanel(
    dateInput('date',
              label = 'Run Date: ',
              value = "2021-03-01",
              min="2019-02-27",#Minimum date of run (002.csv)
              max="2021-03-01" #Maximum date of run (176.csv)
    )
   # PAGE 2
    # dateRangeInput('dateRange',
    #                label = 'Date Range: ',
    #                start = "2021-02-01", end = "2021-03-01",#start on the last month of running
    #                min="2019-02-27",#Minimum date of run (002.csv)
    #                max="2021-03-01" #Maximum date of run (176.csv)
    # )
    ),
  mainPanel(
    leafletOutput("map"),
     # tableOutput("table"),
    textOutput("text")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dataset <- reactive({
    #r<-lapply(my_data, function(x) filter(x, date>=input$dateRange[1]&& date<=input$dateRange[2]))
    r<-lapply(my_data, function(x) filter(x, date==input$date))
    #For loop
    ind<-c()
    j<-1
    for(i in 1:length(my_data)){
       if (length(r[[i]]$date)>2){

         ind[j]<-i
         j<-j+1

       }
    }
#What would I do here to separate the data frames given back ? would I use a lapply ? 
     as.data.frame(my_data[ind])#gives back a list of non-empty dataframes
   })
  
  geodata<-reactive({
    st_as_sf(dataset(),coords = c("lat","lng","elevation"),crs = 4326)
  })
  
  # output$table <- renderTable({
  #   geodata()
  #   })
  
  output$text <- renderText({
    str(geodata())
    hms(geodata()$time[length(geodata()$time)]-geodata()$time[1])

  })
  
 output$map <- renderLeaflet({
   leaflet() %>% addTiles() %>%addPolylines(lng=dataset()$lng, lat=dataset()$lat,col="#FC4C1A",popup="Running Route")#says its not subsetable
 })#Plotted in Strava colors 
   
}
# Run the application 
shinyApp(ui = ui, server = server)

