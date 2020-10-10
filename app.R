library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(ggplot2)

rm(list = ls())

### SETUP ######################################
source("datamanagement.R")
source("fn_actRiskByRegion.R")
source("fn_topStats.R")
source("fn_topCont.R")

################################################

ui <- fluidPage(theme=shinytheme("cosmo"),
                titlePanel("Example of interactive dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Delegate", "Select the portfolio", delFrame$delName, multiple = F,
                                selected = "EU EQ IN"),
                    dateInput("Date", "Select the date", value = max(dataSet$ReportDate), format = "dd-M-yy"),
                    actionButton("button", "Refresh")
                  ), 
                  
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Holdings details", 
                               #textOutput("test"),
                               plotOutput("actRiskRegion"),
                               tableOutput("topContr")),
                      tabPanel("Top details",
                               plotOutput("histRisk"),
                               plotOutput("histFac"))
                  )
                )
                )
)

server <- function(input, output, session) {
  #output$test <- renderText(nrow(fn_topCont(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date)))
  
  output$topContr <- renderTable({
    fn_topCont(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date)
    }, width = "60%")
  
  output$actRiskRegion <- renderPlot({
    fn_actRiskByRegion(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date)
  })
  
  output$histRisk <- renderPlot({
    fn_topStats(delFrame$Delegate[delFrame$delName == input$Delegate])[1]
  })
  
  output$histFac <- renderPlot({
    fn_topStats(delFrame$Delegate[delFrame$delName == input$Delegate])[2]
  })
  
  
}

shinyApp(ui = ui, server = server)