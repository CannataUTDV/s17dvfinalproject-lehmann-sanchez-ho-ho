#ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("ScatterPlot", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Histogram", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Crosstabs, KPIs, Parameters", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Wealth", tabName = "wealthtab", icon = icon("dashboard")),
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabItems(
      
      # End Barchart tab content.
      tabItem(tabName = "wealthtab",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                        # uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("wealthData")
                ),
                tabPanel("Wealth Plot", plotlyOutput("wealthPlot", height=1000))
              )
      ),


      # Begin Box Plots tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("boxplotStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Simple Box Plot", 
                         sliderInput("boxSalesRange1", "Population Range:", # See https://shiny.rstudio.com/articles/sliders.html
                                     min = min(globals$PopulationBelowPovertyLVL), max = max(globals$PopulationBelowPovertyLVL), 
                                     value = c(min(globals$PopulationBelowPovertyLVL), max(globals$PopulationBelowPovertyLVL))),
                         # sliderInput("range5a", "Loop through Quarters:", 
                         #             min(globals$Order_Date), 
                         #             max(globals$Order_Date) + .75, 
                         #             max(globals$Order_Date), 
                         #             step = 0.25,
                         #             animate=animationOptions(interval=2000, loop=T)),
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      # End Box Plots tab content.
      
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"
                                        ), inline=T),
                         sliderInput("KPI1", "KPI_Low:", 
                                     min = 0, max = .1,  value = .1),
                         sliderInput("KPI2", "KPI_Medium:", 
                                     min = .1, max = .15,  value = .15),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Crosstab", plotOutput("plot1", height=1000))
              )
      ),
      # End Crosstab tab content.
      
      
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("histogramData1")
                ),
                tabPanel("Simple Histogram", plotlyOutput("histogramPlot1", height=1000))
              )
      ),
      # End Histograms tab content.
      
      # Begin Scatter Plots tab content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterData1")
                ),
                tabPanel("Simple Scatter Plot", plotlyOutput("scatterPlot1", height=1000))
              )
      ),
      # End Scatter Plots tab content.
      
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL"
                                      ), inline=T),
                         uiOutput("counties2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is the data of the poverty percentage by each state',
                         hr(),
                         DT::dataTableOutput("barchartData1")
                ),
                tabPanel("Poverty on each state by race", plotOutput("barchartPlot1", height=1500)),
                tabPanel("Unites States Poverty by State", leafletOutput("barchartMap1", height = 800))
              )
      )
      # End Barchart tab content.
    )
  )
)
