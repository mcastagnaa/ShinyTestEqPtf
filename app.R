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
source("fn_HstRiskSplit.R")
source("fn_topStats.R")
source("fn_topCont.R")
source("fn_mVaR.R")
source("fn_fundamentals.R")
source("fn_quintiles.R")
source("fn_topTable.R")
source("fn_MktVal.R")

################################################

ui <- fluidPage(theme=shinytheme("lumen"),
                fluidRow(
                  column(2,br(),
                         img(src = "logo.png", width = "100")),
                  column(10,
                         titlePanel("Example of interactive dashboard"),
                         h4("Please select either a Thursday or the last available date"),
                         h5(tags$em("Equity EU Income has all the dates")),
                         hr())
                ),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    selectInput("Delegate", "Portfolio:", dropDownSel$Name, multiple = F,
                                selected = "Equity EU Income"),
                    hr(),
                    dateInput("Date", "Date:", 
                              value = max(dataSet$ReportDate), 
                              format = "dd-M-yy",
                              weekstart = 1),
                    hr(),
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
                                 selected = "Region"),
                  width = 3), 
                mainPanel = mainPanel(
                    fluidRow(
                      column(5,
                             plotOutput("mktVal", inline = F),
                             offset = 1),
                      column(6, br(),
                             tableOutput("topTable"),
                             offset = 0)),
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("Holdings details",
                               #textOutput("test"),
                               plotOutput("actRiskSplit"),
                               h4("List of NA lines"),
                               tableOutput("actRiskNA"),
                               br(),
                               plotOutput("hstRiskSplit"),
                               br(),
                               h4("List of top contributors/detractors"),
                               tableOutput("topContr")),
                      tabPanel("Ex-ante risk details",
                               plotOutput("histRisk"),
                               plotOutput("histFac")),
                      tabPanel("Marginal VaR",
                               p(strong("MarginalVaRMCPort"), "measures the change in total VaR",
                                 "from taking an additional dollar of exposure (x100)",
                                 "to a given security."),
                               p(strong("PartialVaRMCPort"), "measures the change in total VaR",
                                 "from completely removing the position."),
                               dataTableOutput("mVaR")),
                      tabPanel("Fundamentals",
                               plotlyOutput("fundChart", inline = F, height = "130%"),
                               br(),
                               h4(em("Quintile buckets analysis")),
                               plotlyOutput("quintiles", inline = F, height = "130%")),
                      tabPanel("NOTES",
                               h2("Methodological notes, data sources, criteria"),
                               #p(code("This is where all the notes about what each object represents would fit.")),
                               h3("Basic metrics"),
                               p(em("TotalRisk"),"and", em("VaRMC"), "are annualized, forward looking from Bloomberg regional model MAC2."),
                               p(em("TotalRisk Diff"),"is the annualized expected Tracking Error."),
                               p(em("DelCode"), "(no decimals!) is the portfolio code on Bloomberg."),
                               p(em("MktVal"), "is the value of the money managed by that sleeve for the main class."))
                    )
                    , width = 9)
)
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
    fn_actRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                    input$Date, input$Split)[1]
  })
  
  output$hstRiskSplit <- renderPlot({
    fn_HstRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Split)
  })
  
  output$actRiskNA <- renderTable({
    fn_actRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                    input$Date, input$Split)[2]
  })
  
  output$mktVal <- renderPlot({
    fn_mktVal(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])
  }, height = 300)
  
  output$histRisk <- renderPlot({
    fn_topStats(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])[1]
  })
  
  output$histFac <- renderPlot({
    fn_topStats(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])[2]
  })
  
  output$mVaR <- renderDataTable({
    fn_mVaR(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
  })
  
  output$fundChart <- renderPlotly({
    fn_fundamentals(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])
  })
  
  output$quintiles <- renderPlotly({
    fn_quintiles(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
  })
  
  output$topTable <- renderTable({ 
    fn_topTable(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)
  }, bordered = T, spacing = "s", na = "", striped = T)
}

shinyApp(ui = ui, server = server)