library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

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
                # absolutePanel(id = "choices", 
                #               titlePanel("Example of interactive dashboard"),
                #               top = 0, left = "10%", height = "30%", width = "100%", fixed = T,
                    fluidRow(
                      column(3, 
                             selectInput("Delegate", "Select the portfolio", dropDownSel$Name, multiple = F,
                                         selected = "Equity EU Income"),
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
                                          "Asset Class" = "AssetClass",
                                          "Instrument" = "Instrument",
                                          "Issuer" = "Issuer",
                                          "Market Cap" = "MktCap",
                                          "Rating" = "Rating",
                                          "IG/HY" = "RatingGrp"), 
                                        selected = "Region", width = "60%")
                    )
                    #)
                    ),
                
                # absolutePanel(id = "main",
                #               top = "40%", height = "80%", width = "100%", fixed = F,
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
                               plotlyOutput("fundChart", inline = F, height = "130%"),
                               plotlyOutput("quintiles", inline = F, height = "130%")),
                      tabPanel("Ratings")
                    )
                  )
                #)
)

server <- function(input, output, session) {
  #output$test <- renderText(input$Split)
  
  output$mVaR1 <- renderText(HTML(paste("<b>MarginalVaRMCPort</b> measures the change in total VaR",
                   "from taking an additional dollar of exposure (x100)",
                   "to a given security.")))
  
  output$mVaR2 <- renderText(HTML(paste("<b>PartialVaRMCPort</b> measures the change in total VaR",
                   "from completely removing the position.")))
  
  output$topContr <- renderTable({
    fn_topCont(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
    }, width = "100%")
  
  output$actRiskSplit <- renderPlot({
    fn_actRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date, input$Split)
  })
  
  output$histRisk <- renderPlot({
    fn_topStats(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])[1]
  })
  
  output$histFac <- renderPlot({
    fn_topStats(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])[2]
  })
  
  output$mVaR <- renderTable({
    fn_mVaR(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
  }, width = "100%")
  
  output$fundChart <- renderPlotly({
    fn_fundamentals(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])
  })
  
  output$quintiles <- renderPlotly({
    fn_quintiles(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
  })
}

shinyApp(ui = ui, server = server)