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
source("fn_topTable.R")

################################################

ui <- fluidPage(theme=shinytheme("cosmo"),
                titlePanel("Example of interactive dashboard"),
                h4("Please select either a Friday or the last available date"),
                h5(tags$em("Equity EU Income has all the dates")),
                hr(),
                fluidRow(column(8,
                                fluidRow(
                                  column(8, 
                                         selectInput("Delegate", "Portfolio:", dropDownSel$Name, multiple = F,
                                                     selected = "Equity EU Income"),
                                         verbatimTextOutput("topTable")
                                  ),
                                  column(4,
                                         dateInput("Date", "Date:", 
                                                   value = max(dataSet$ReportDate), 
                                                   format = "dd-M-yy",
                                                   weekstart = 1)
                                  )
                                )),
                         column(4, 
                                radioButtons("Split", "Group by:",
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
                                             selected = "Region", width = "60%"))),
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
                               p(strong("MarginalVaRMCPort"), "measures the change in total VaR",
                                      "from taking an additional dollar of exposure (x100)",
                                      "to a given security."),
                               p(strong("PartialVaRMCPort"), "measures the change in total VaR",
                                      "from completely removing the position."),
                               tableOutput("mVaR")),
                      tabPanel("Fundamentals",
                               plotlyOutput("fundChart", inline = F, height = "130%"),
                               plotlyOutput("quintiles", inline = F, height = "130%"))
                    )
                  )
                #)
)

server <- function(input, output, session) {
  # output$test <- renderText(
  #   max(dataSet$ReportDate[dataSet$Delegate == dropDownSel$DelCode[dropDownSel$Name == input$Delegate]])
  # )
  
  observeEvent(input$Delegate, {
    updateDateInput(session, "Date", 
                    value = input$Date,
                    min = min(dataSet$ReportDate[dataSet$Delegate == 
                                                   dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]),
                    max = max(dataSet$ReportDate[dataSet$Delegate ==
                                                   dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]))
   }) 
  
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
  
  output$topTable <- renderPrint({ 
    fn_topTable(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
  })
}

shinyApp(ui = ui, server = server)