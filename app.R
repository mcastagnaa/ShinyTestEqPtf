library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

### SETUP ######################################
source("datamanagement.R")
source("fn_actRiskSplit.R")
source("fn_topStats.R")
source("fn_topCont.R")
source("fn_mVaR.R")
source("fn_fundamentals.R")
source("fn_quintiles.R")

################################################

ui <- fluidPage(theme=shinytheme("cosmo"),
                titlePanel("Example of interactive dashboard"),
                  fluidRow(column(2, 
                                  selectInput("Delegate", "Select the portfolio", delFrame$delName, multiple = F,
                                              selected = "EU EQ IN"),
                                  #actionButton("button", "Refresh"),
                  ),
                  column(2,
                         dateInput("Date", "Select the date", value = max(dataSet$ReportDate), format = "dd-M-yy")
                         ),
                  column(3, 
                         radioButtons("Split", "Grouping by:",
                                      c("Region" = "Region",
                                        "Country" = "CtryName",
                                        "Developed" = "MSCI_DC",
                                        "CCY" = "Crncy",
                                        "Sector" = "Sector",
                                        "GICS Sector" = "GICSSectorName",
                                        "GICS Industry" = "GICSIndustryName",
                                        "SecurityType" = "SecurityType",
                                        "Market Cap" = "MktCap"), selected = "Region")
                         ))
                    , 
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Holdings details", 
                               #textOutput("test"),
                               plotOutput("actRiskSplit"),
                               tableOutput("topContr")),
                      tabPanel("Top details",
                               plotOutput("histRisk"),
                               plotOutput("histFac")),
                      tabPanel("Marginal VaR",
                               uiOutput("mVaR1"),
                               uiOutput("mVaR2"),
                               tableOutput("mVaR")),
                      tabPanel("Fundamentals",
                               plotOutput("fundChart"),
                               plotOutput("quintiles"))
                  )
                )
)

server <- function(input, output, session) {
  #output$test <- renderText(input$Split)
  
  output$mVaR1 <- renderText(HTML(paste("<b>MarginalVaRMCPort</b> measures the change in total VaR",
                   "from taking an additional dollar of exposure (x100)",
                   "to a given security.")))
  
  output$mVaR2 <- renderText(HTML(paste("<b>PartialVaRMCPort</b> measures the change in total VaR",
                   "from completely removing the position.")))
  
  output$topContr <- renderTable({
    fn_topCont(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date)
    }, width = "100%")
  
  output$actRiskSplit <- renderPlot({
    fn_actRiskSplit(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date, input$Split)
  })
  
  output$histRisk <- renderPlot({
    fn_topStats(delFrame$Delegate[delFrame$delName == input$Delegate])[1]
  })
  
  output$histFac <- renderPlot({
    fn_topStats(delFrame$Delegate[delFrame$delName == input$Delegate])[2]
  })
  
  output$mVaR <- renderTable({
    fn_mVaR(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date)
  }, width = "100%")
  
  output$fundChart <- renderPlot({
    fn_fundamentals(delFrame$Delegate[delFrame$delName == input$Delegate])
  })
  
  output$quintiles <- renderPlot({
    fn_quintiles(delFrame$Delegate[delFrame$delName == input$Delegate], input$Date)
  })
}

shinyApp(ui = ui, server = server)