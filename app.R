library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

rm(list = ls())

options(dplyr.summarise.inform = FALSE)

### SETUP ######################################
source("datamanagement.R")
#load("datadump.Rda")

source("fn_actRiskSplit.R")
source("fn_HstRiskSplit.R")
source("fn_topStats.R")
source("fn_topCont.R")
source("fn_mVaR.R")
source("fn_fundamentals.R")
source("fn_quintiles.R")
source("fn_topTable.R")
source("fn_MktVal.R")
source("fn_scenarios.R")
source("fn_factors.R")
source("fn_factNonFact.R")
source("fn_LineCounts.R")
source("fn_DivBnft.R")
source("fn_Rets.R")

################################################


ui <- fluidPage(theme=shinytheme("lumen"),
                fluidRow(
                  column(2,br(),
                         img(src = "logo.png", width = "100")),
                  column(10,
                         titlePanel("Example of interactive dashboard"),
                         h4("Please select either a Thursday or the last available date"),
                         h5(tags$em("Equity Global Blend has all the dates")),
                         hr())
                ),
                sidebarLayout(
                  sidebarPanel = sidebarPanel(
                    selectInput("Delegate", "Portfolio:", dropDownSel$Name, multiple = F,
                                selected = "Equity GL Blend"),
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
                               plotOutput("factNonFact"),
                               h4("List of NA lines"),
                               tableOutput("actRiskNA"),
                               br(),
                               plotOutput("hstRiskSplit"),
                               br(),
                               h4("List of top contributors/detractors"),
                               tableOutput("topContr")),
                      tabPanel("Returns",
                               br(),
                               dateInput("retStDate", "Returns start date:", 
                                         value = min(retsSet$Date), 
                                         format = "dd-M-yy",
                                         weekstart = 1),
                               br(),
                               plotOutput("retChart")),
                      tabPanel("Ex-ante risk details",
                               plotOutput("histRisk"),
                               plotOutput("histFac"),
                               hr(),
                               h4("Individual factor contribution analysis"),
                               dataTableOutput("factors")),
                      tabPanel("Diversification",
                               plotOutput("histLineC"),
                               plotOutput("histDivBnft"),
                               hr(),
                               h4("Individual lines correlation analysis"),
                               p("Example for", strong("US income portfolio, as of 26-Feb-2021 (Yahoo! finance as data source)"),
                                 "calculated over 52 weekly relative returns"),
                               p(""),
                               img(src = "corrPlotEx.png", width = "900"),
                               plotOutput("corrChart_expl")),
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
                      tabPanel("Scenarios",
                               plotOutput("scenDate"),
                               tableOutput("scenDes")),
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

  observeEvent(input$Delegate, {
    updateDateInput(session, "Date", 
                    value = input$Date,
                    min = min(dataSet$ReportDate[dataSet$Delegate == 
                                                   dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]),
                    max = max(dataSet$ReportDate[dataSet$Delegate ==
                                                   dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]))
    updateDateInput(session, "retStDate", 
                    value = min(retsSet$Date[retsSet$DelCode ==
                                               dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]),
                    min = min(retsSet$Date[retsSet$DelCode ==
                                             dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]),
                    max = max(retsSet$Date[retsSet$DelCode ==
                                             dropDownSel$DelCode[dropDownSel$Name == input$Delegate]]))
   }) 
  
  ### Holdings details tab #################
  output$topContr <- renderTable(fn_topCont(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date), 
                                 width = "100%")
  output$actRiskSplit <- renderPlot(fn_actRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                                                    input$Date, input$Split)[1])
  output$factNonFact <- renderPlot(fn_factNonFact(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                                                  input$Date, input$Split))
  output$hstRiskSplit <- renderPlot(fn_HstRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                                                    input$Split))
  output$actRiskNA <- renderTable(fn_actRiskSplit(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                                                  input$Date, input$Split)[2])
  
  ### Top panel #############################
  output$mktVal <- renderPlot(fn_mktVal(dropDownSel$DelCode[dropDownSel$Name == input$Delegate]), 
                              height = 300)
  output$topTable <- renderTable(fn_topTable(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], 
                                             input$Date), 
                                 bordered = T, spacing = "s", na = "", striped = T)
  
  ### Returns ###############################
  output$retChart <- renderPlot(fn_Rets(delCode = dropDownSel$DelCode[dropDownSel$Name == input$Delegate],
                                        startDate = input$retStDate,
                                        endDate = input$Date))
  
  ### Ex-ante risk details tab #############
  output$histRisk <- renderPlot(fn_topStats(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])[1])
  output$histFac <- renderPlot(fn_topStats(dropDownSel$DelCode[dropDownSel$Name == input$Delegate])[2])
  output$factors <- renderDataTable(fn_factors(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date),
                                    options = list(pageLength = 10))
  
  ### Diversification #############
  output$histLineC <- renderPlot(fn_LineCounts(dropDownSel$DelCode[dropDownSel$Name == input$Delegate]))
  output$histDivBnft <- renderPlot(fn_DivBnft(dropDownSel$DelCode[dropDownSel$Name == input$Delegate]))
    
  ### mVar tab #############################
  output$mVaR <- renderDataTable(fn_mVaR(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date))
  
  ### fundamentals tab #####################
  output$fundChart <- renderPlotly(fn_fundamentals(dropDownSel$DelCode[dropDownSel$Name == input$Delegate]))
  output$quintiles <- renderPlotly(fn_quintiles(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date))
  
  ### scenario tab #########################
  output$scenDate <- renderPlot(fn_scen(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)[1])
  output$scenDes <- renderTable(fn_scen(dropDownSel$DelCode[dropDownSel$Name == input$Delegate], input$Date)[2],
                                striped = T,
                                width = "80%")

}

shinyApp(ui = ui, server = server)