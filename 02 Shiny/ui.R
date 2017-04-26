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
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabItems(
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
                         'Here is data for Female and Male Povery along with Education by County',
                         hr(),
                         DT::dataTableOutput("barchartData1"),
                         hr(),
                         'Here is data Poverty and Health Insurance Males',
                         hr(),
                         DT::dataTableOutput("barchartData3")
                ),
                tabPanel("Barchart for Undergrad Females and Poverty ","Black: Total Number of FemalePov18to24 per County. Blue :(FemalePov18to24 - FemaleUnderGrad). Red: Average betwen all counties", plotOutput("barchartPlot1", height=2600)),
                tabPanel("Barchart for Undergrad Males and Poverty ","Black: Total Number of MalePov18to24 per County. Blue :(MalePov18to24 - MaleUnderGrad). Red: Average betwen all counties", plotOutput("barchartPlot2", height=2600)),
                tabPanel("Poverty and Health Insurance Males", plotlyOutput("barchartPlot3", height=2800) )
              )
      )
      # End Barchart tab content.
    )
  )
)
