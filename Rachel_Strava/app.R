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
library(sf)
library(shinythemes)

#Load data
my_data <- readRDS("~/2021/EDA/EDA workspace/EDA_Assignment3/Rachel_Strava/data/my_data.rds")#Is this a probelm 
#avgrun<-c(3,4,5)
# handling the data conversions 
my_data<- lapply(my_data, function(x) {
  x$date <- ymd(x$date)
  x$time<-hms(x$time)
  x$time<-seconds(x$time)
  x #why this ?
})

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("Rachel's Strava", theme = shinytheme("united"),
             tabPanel("Activity Data", fluid = TRUE, icon = icon("running"),
  #PAGE 1 
  sidebarLayout(
  sidebarPanel(
    
    titlePanel("Run Statistics"),
    
    
                    
    #Select Date of Run to plot                 
    dateInput('date',
              label = 'Run Date: ',
              value = "2021-03-01",
              min="2019-02-27",#Minimum date of run (002.csv)
              max="2021-03-01" #Maximum date of run (176.csv)
    )
    # Select which Data Metrics to display
    # checkboxGroupInput(inputId = "Run_data",
    #                    label = "Select Run Statistics:",
    #                    choices = c("Distance","Moving Time","Elevation Gain","Average Pace"),
    #                    selected = c("Distance","Moving Time","Elevation Gain","Average Pace"))
   
    ),
  mainPanel(
    #Draw Map 
    leafletOutput("map"),
    textOutput("act_t"),
    textOutput("dist"),
    textOutput("elv"),
    textOutput("pace")

    )
)),

tabPanel("Activity Comparisons", fluid = TRUE, icon = icon("chart-bar"),
         titlePanel("Activity Breakdown "),
           mainPanel(
             #Display Table of Data Metrics
             dataTableOutput("kmtable"),
             textOutput("texty")
           )
        )
)
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Work with Data 
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
     as.data.frame(my_data[ind])#gives back a list of non-empty dataframes
    
     #elevationgain<-
   })
  #Work with geo data 
  geodata<-reactive({
    m<-st_as_sf(dataset(),coords = c("lat","lng","elevation"),crs = 4326)
    my_crs <- "+proj=utm +zone=34H +datum=WGS84 +units=m +no_defs" #for Beaufort 
    sights<-st_transform(m, crs = my_crs)
    
    dist<-c()
    for(i in 1:(length(sights$geometry)-1)){
      dist[i]<-st_distance(sights$geometry[i],sights$geometry[i+1])#10m from 
    }
    
    dist1<-c(0,dist)
    sights$dist<-dist1
    
    sights_test<-sights%>%mutate(tot_dist=cumsum(dist)/1000) #in meters 
    sights_test
  
  })

  #Work out averages 
  tot_distance<-reactive({
    round(geodata()$tot_dist[length(geodata()$tot_dist)],2)
  })
  
  acttime<- reactive({
    round((dataset()$time@.Data[length(dataset()$time@.Data)]-dataset()$time@.Data[1])/60,2)
  })
  
  elev<-reactive({
    max(dataset()$elevation)-min(dataset()$elevation)
  })
  
  splits<- reactive({
    km<-seq(from=1,to=round(tot_distance()),by=1)#initilize coloumn one of dataframe = the kms 
    splittime<-c()
    for(i in 1:round(tot_distance())){
      new<-geodata()%>%filter(tot_dist<i & tot_dist>=(i-1)) #filter out the data between the two kms relevent = 1km split 
      splittime[i]<-round((new$time@.Data[length(new$time@.Data)]-new$time@.Data[1])/60,2)
       
    }
    df<-as.data.frame(cbind(km,splittime))
    df
  })
  
  
#__________________________________________________________________________________________________________
  
  output$kmtable <- renderDataTable({
    splits()
    })
  
  output$texty<-renderText({
    splits()
  })

  #Renders Text
  output$act_t <- renderText({
    paste("Total Activity Time:" ,acttime(), "mins")
  })
  
  output$dist <- renderText({
    paste("Total Distance: ",tot_distance(), "km")
  })
  
  output$elv <- renderText({
    paste("Total Elevation Gain: ",elev(), "m")
  })
  
  output$pace <- renderText({
    paste("Average Pace :" ,round(acttime()/tot_distance(),2), "min /km" )
  })
  
  #Output Map 
 output$map <- renderLeaflet({
   leaflet() %>% addTiles() %>%addPolylines(lng=dataset()$lng, lat=dataset()$lat,col="#FC4C1A",popup="Running Route")%>%addMiniMap(position = "bottomleft")#says its not subsetable
 })#Plotted in Strava colors 
   
}


# Run the application 
shinyApp(ui = ui, server = server)

