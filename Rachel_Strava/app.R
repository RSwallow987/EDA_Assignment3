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
library(spData)
library(dplyr)
library(tidyverse)
library(leaftime)

#Load data
my_data <- readRDS("~/2021/EDA/EDA workspace/EDA_Assignment3/Rachel_Strava/data/my_data.rds")#Is this a probelm 
x<-c(1,2,3,4,5)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),
  titlePanel("My Strava App"),
  selectInput("run", label = "Run", choices = x),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dataset <- reactive({
    my_data[[as.numeric(input$run)]]
  })
  
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

