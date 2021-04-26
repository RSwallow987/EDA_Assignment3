# This application does the following:
# a) allows the user to map their run i.e. to visualize their run on a map,
# b) provides the user with different summary statistics about their runs 
# and allows them to interrogate the dataset of their runs in different ways. 
# c) The app is accompanied by documentation explaining how to use the app and giving a short worked example demonstrating the app.


#libraries for ui and sever
library(shiny)
library(shinythemes)
#Data Filtering 
library(dplyr)
library(tidyverse)
#Mapping data and distances
library(leaflet)
library(leaftime)
library(spData)
library(sf)
#Working with Time and Dates
library(lubridate)

#Load data
my_data <- readRDS("~/2021/EDA/EDA workspace/EDA_Assignment3/Rachel_Strava/data/my_data.rds")#Is this a probelm 

# handling the data conversions 
my_data<- lapply(my_data, function(x) {
  x$date <- ymd(x$date) #Convert str date to a lubridate date 
  x$time<-hms(x$time)# Convert str time to a lubridate time 
  x$time<-seconds(x$time) #Convert time to a total seconds 
  x 
})

# Define UI for application
ui <- fluidPage(
  
  navbarPage("Rachel's Strava", theme = shinytheme("united"), #Add a navigation panel with a nice theme 
             tabPanel("Activity Data", fluid = TRUE, icon = icon("running"), #Add icons for nav sections 
  #PAGE 1 
  sidebarLayout( #define sideoanel layout 
  sidebarPanel(
    
    titlePanel("Run Statistics"), #Give a title to the side bar and Page 
    
    
                    
    #User can select date of the run they want to plot                
    dateInput('date',
              label = 'Pick a run date: ', #guide the user to pick a date of the run they want to analyze
              value = "2021-03-01", #Start date is set as the last run 
              min="2019-02-27",#Minimum date of run (002.csv)
              max="2021-03-01" #Maximum date of run (176.csv)
    )
    ),
  mainPanel( #in the main panel of the first page plot
    #Draw Map 
    leafletOutput("map"), #UI Map
    #Summary Statistics 
    textOutput("act_t"),
    textOutput("dist"),
    textOutput("elv"),
    textOutput("pace")

    )
)),

tabPanel("Activity Splits", fluid = TRUE, icon = icon("chart-bar"), #Label the second nav page 
         titlePanel("Activity Breakdown "), 
           mainPanel(
             #Display Table of Data Metrics
             dataTableOutput("kmtable")#interactive data table for splits
           )
        )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dataset <- reactive({
    r<-lapply(my_data, function(x) filter(x, date==input$date)) #Select the dataframe in the list with the matching date 
    #Filter out all the dataframes in the list with number of rows smaller than 2. Remaining list will only contain the dataframe of the run. 
    ind<-c()
    j<-1
    for(i in 1:length(my_data)){
       if (length(r[[i]]$date)>2){

         ind[j]<-i
         j<-j+1

       }
    }
     as.data.frame(my_data[ind])#gives back a list of non-empty dataframes
   })
  
  #Work with geo data 
  geodata<-reactive({
    #m<-st_as_sf(dataset(),coords = c("lat","lng","elevation"),crs = 4326)
    m<-st_as_sf(dataset(),coords = c("lng","lat"),crs = 4326) #Projection for distance calculations 
    my_crs <- "+proj=utm +zone=34S +datum=WGS84 +units=m +no_defs" #for Beaufort 
    sights<-st_transform(m, crs = my_crs) #transform 
    
    
    dist<-c()
    for(i in 1:(length(sights$geometry)-1)){
      dist[i]<-st_distance(sights$geometry[i],sights$geometry[i+1])#Calculate the distance between points 
    }
    
    dist1<-c(0,dist)#replace index 1 with a zero 
    sights$dist<-dist1 #Add the coloumn to sights 
    
    sights_test<-sights%>%mutate(tot_dist=cumsum(dist)/1000) #cumulative sum in km
    sights_test #return dataframe 
  
  })

  #Work out averages 
  tot_distance<-reactive({
    round(geodata()$tot_dist[length(geodata()$tot_dist)],2)#get last entry - total distance 
  })
  
  acttime<- reactive({
    round((dataset()$time@.Data[length(dataset()$time@.Data)]-dataset()$time@.Data[1])/60,2) #Difference in seconds between first point and last point divided by 60 = minutes 
  })
  
  elev<-reactive({
    max(dataset()$elevation)-min(dataset()$elevation)#Change in elevation 
  })
  
  splits<- reactive({
    km<-seq(from=1,to=round(tot_distance()),by=1)#initilize coloumn one of dataframe = the kms 
    splittime<-c()#empty list 
    for(i in 1:round(tot_distance())){ #for kms of run 
      new<-geodata()%>%filter(tot_dist<i & tot_dist>=(i-1)) #filter out the data between the two kms relevent = 1km split 
      splittime[i]<-round((new$time@.Data[length(new$time@.Data)]-new$time@.Data[1])/60,2)#time differences between the two datapoints representing the km difference 
       
    }
    df<-as.data.frame(cbind(km,splittime))#return a dataframe 
    df
  })
  
  
# PAGE 1__________________________________________________________________________________________________________

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
 
#PAGE 2_________________________________________________________________________________________________________
 
 output$kmtable <- renderDataTable({ #Plot a reactive table 
   splits()
 },
 options = list(  columns = list(
   list(title = 'Kilometer Split'),
   list(title = 'Split time (min)')
 ))
 )
 
}

# Run the application 
shinyApp(ui = ui, server = server)

