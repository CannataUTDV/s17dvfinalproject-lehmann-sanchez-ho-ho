# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)
library(reshape2)
library(geojson)
library(geojsonio)
library(ggplot2)
library(tidyr)

online0 = TRUE

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
if(online0) {
  states = query(
    data.world(propsfile = "www/.data.world"),
    dataset="lordlemon/s-17-edv-final-project", type="sql",
    query="select StateName as S from PovertyUSAStates"
  )  #%>% View()
}
states_list <- as.list(states$S, states$S)
states_list <- append(list("All" = "All"), states_list)
#region_list5 <- region_list

############################### Start shinyServer Function ####################



shinyServer(function(input, output) {   
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  output$boxplotStates <- renderUI({selectInput("selectedBoxplotStates", "Choose States:",
                                                states_list, multiple = TRUE, selected='All') })
  
  # These widgets are for the Histogram tab.
  online4 = reactive({input$rb4})
  
  # These widgets are for the Scatter Plots tab.
  online3 = reactive({input$rb3})
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$counties2 <- renderUI({selectInput("selectedStates", "Choose States:", states_list, multiple = TRUE, selected='All') })
  
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  
  # Begin Box Plot Tab ------------------------------------------------------------------
  dfbp1 <- eventReactive(input$click5, {
    if(input$selectedBoxplotStates == 'All') states_list <- input$selectedBoxplotStates
    else states_list <- append(list("Skip" = "Skip"), input$selectedBoxplotStates)
    if(online5() == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select StateName, WhitePopulation,BlackPopulation,LatinoHispanic,WhitePopulationBelowPovertyLVL,BlackPopulationBelowPovertyLVL,LatinoHispanicBelowPovertyLVL from PovertyUSAStates
        where (? = 'All' or StateName in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))",
        queryParameters = states_list ) #%>% View()
    }
  })
  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  dfbp2 <- eventReactive(c(input$click5, input$boxSalesRange1), {
    dfbp1() %>% dplyr::filter(WhitePopulation >= input$boxSalesRange1[1] & WhitePopulation <= input$boxSalesRange1[2]) # %>% View()
  })
  # 
  # dfbp3 <- eventReactive(c(input$click5, input$range5a), {
  #   dfbp2() %>% dplyr::filter(lubridate::year(Order_Date) == as.integer(input$range5a) & lubridate::quarter(Order_Date) == (4 * (input$range5a - as.integer(input$range5a))) + 1) %>% dplyr::arrange(desc(Order_Date)) # %>% View()
  # })
  
  output$boxplotPlot1 <- renderPlotly({
    #View(dfbp3())
    dat_m <- dfbp2()[, c("StateName","WhitePopulation","WhitePopulationBelowPovertyLVL","BlackPopulation","BlackPopulationBelowPovertyLVL","LatinoHispanic","LatinoHispanicBelowPovertyLVL")]
    dat_m <- melt(dat_m, id.vars = c("StateName"),
                  measure.vars = c("WhitePopulationBelowPovertyLVL","WhitePopulation","BlackPopulationBelowPovertyLVL","BlackPopulation","LatinoHispanicBelowPovertyLVL","LatinoHispanic"))
    #print(dat_m)
    p <- ggplot(dat_m) + 
      geom_boxplot(aes(x=StateName, y=value, colour=variable)) + 
      #ylim(0, input$boxSalesRange1[2]) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))+
      labs(x="States",y="Population")
    ggplotly(p)
  })
  # End Box Plot Tab ___________________________________________________________
  
  # Begin Crosstab Tab ------------------------------------------------------------------
  dfct1 <- eventReactive(input$click1, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select StateName,PopulationPolled,WhitePopulation,BlackPopulation,LatinoHispanic, (WhitePopulationBelowPovertyLVL+BlackPopulationBelowPovertyLVL+LatinoHispanicBelowPovertyLVL) as sum_poverty,  (WhitePopulationBelowPovertyLVL+BlackPopulationBelowPovertyLVL+LatinoHispanicBelowPovertyLVL)/PopulationPolled as ratio,
case
when (WhitePopulationBelowPovertyLVL+BlackPopulationBelowPovertyLVL+LatinoHispanicBelowPovertyLVL)/PopulationPolled < ? then '03 Low'
when (WhitePopulationBelowPovertyLVL+BlackPopulationBelowPovertyLVL+LatinoHispanicBelowPovertyLVL)/PopulationPolled < ? then '02 Medium'
else '01 High'
end AS kpi
from PovertyUSAStates",
        queryParameters = list(KPI_Low(), KPI_Medium())
      )  #%>% View()
    }
  })
  output$data1 <- renderDataTable({DT::datatable(dfct1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({
    dat_m <- dfct1()[, c("StateName","WhitePopulation","BlackPopulation","LatinoHispanic", "ratio", "kpi" )]
    dat_m <- melt(dat_m, id.vars = c("StateName","kpi","ratio"),
                  measure.vars = c("WhitePopulation","BlackPopulation","LatinoHispanic"))
    ggplot(dat_m) + 
      theme(axis.text.x=element_text(size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_text(aes(x=variable, y=StateName, label=value), size=6) +
      geom_tile(aes(x=variable, y=StateName, fill=kpi), alpha=0.50)+
      labs(y="States",x="Ethnicity")
  })
  # End Crosstab Tab ___________________________________________________________
  
  # Begin Histgram Tab ------------------------------------------------------------------
  dfh1 <- eventReactive(input$click4, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select StateName, HispanicLatinoAbove200 from IncomeAbove200"
      )  #%>% View()
    }
    })
  
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1(), rownames = FALSE,
                                                          extensions = list(Responsive = TRUE, 
                                                                            FixedHeader = TRUE)
  )
  })
  
  output$histogramPlot1 <- renderPlotly({p <- ggplot(dfh1()) +
    geom_histogram(aes(x=HispanicLatinoAbove200)) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  ggplotly(p)
  })
  # End Histogram Tab ___________________________________________________________
  
  
  
  
  # Begin Scatter Plots Tab ------------------------------------------------------------------
  dfsc1 <- eventReactive(input$click3, {
    if(online3() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select p.StateName as State, p.PopulationBelowPovertyLVL as PovertyPopulation,(h.MalePopulationNoHI + h.FemalePopulationNoHI) as TotalPopulationNOHI, h.MalePopulationNoHI as MaleNoHI, h.FemalePopulationNoHI as FemaleNoHI from PovertyUSAStates p inner join USAHealthInsurance h on p.StateName = h.StateName"
      ) # %>% View()
    }

  })
  output$scatterData1 <- renderDataTable({DT::datatable(dfsc1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  output$scatterPlot1 <- renderPlotly({
    dat_m <- dfsc1()[, c("State","PovertyPopulation","TotalPopulationNOHI","MaleNoHI", "FemaleNoHI" )]
    dat_m <- melt(dat_m, id.vars = c("State","TotalPopulationNOHI"),
              measure.vars = c("MaleNoHI","FemaleNoHI"))
   # print(dat_m)
    p <- ggplot(dat_m, aes(TotalPopulationNOHI, value, colour = variable, state)) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(label = State), size=2) +
      stat_summary(aes(y = value,group=2), fun.y=mean, colour="red", geom="line",group=2) +
      labs(x="Total Populaton with no HI",y="Amount of people")

  ggplotly(p)
  
  })
  # End Scatter Plots Tab ___________________________________________________________
  
  
  # Begin Barchart Tab ------------------------------------------------------------------
  dfbc1 <- eventReactive(input$click2, {
    if(input$selectedStates == 'All') states_list <- input$selectedStates
    else selectedStates <- append(list("Skip" = "Skip"), input$selectedStates)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select p.StateName as StateName, p.PopulationPolled as PopulationPolled, ( p.WhitePopulationBelowPovertyLVL/p.WhitePopulation) as WhitePovertyPercent,  ( p.BlackPopulationBelowPovertyLVL/p.BlackPopulation) as BlackPovertyPercent,  ( p.LatinoHispanicBelowPovertyLVL/p.LatinoHispanic) as LatinoPovertyPercent
from PovertyUSAStates p inner join IncomeAbove200 i on p.StateName = i.StateName
        group by p.WhitePopulation
      ", queryParameters = "p.StateName"
        
      ) #%>% View()
    }
  })
  
  mapDataQuery <-  eventReactive(input$click2, {
    if(input$selectedStates == 'All') states_list <- input$selectedStates
    else selectedStates <- append(list("Skip" = "Skip"), input$selectedStates)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select p.StateName as StateName, (PopulationBelowPovertyLVL/p.PopulationPolled) as PopulationPovertyPercent, ( p.WhitePopulationBelowPovertyLVL/p.WhitePopulation) as WhitePovertyPercent,  ( p.BlackPopulationBelowPovertyLVL/p.BlackPopulation) as BlackPovertyPercent,  ( p.LatinoHispanicBelowPovertyLVL/p.LatinoHispanic) as LatinoPovertyPercent
from PovertyUSAStates p inner join IncomeAbove200 i on p.StateName = i.StateName
        group by p.WhitePopulation
        ", queryParameters = "p.StateName"
        
      ) #%>% View()
    }
  })
  
  output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })

  #barchart
  output$barchartPlot1 <- renderPlot({
    dat_m <- dfbc1()[, c("StateName","WhitePovertyPercent","BlackPovertyPercent","LatinoPovertyPercent")]
    dat_m <- (melt(dat_m, id.vars = "StateName"))
    ggplot(dat_m, aes(x=StateName, y=value, fill=variable)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity",position = "dodge")+
      labs(x="States",y="Percent Below Poverty Level")+
      #facet_wrap(~County, ncol=4) + 
      coord_flip()+
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      #geom_text(mapping=aes(x=StateName, y=value, label=(value)),colour="black", hjust=.5,vjust=.5) +
     # geom_text(mapping=aes(x=County, y=BlackPovertyPercent, label=round)),colour="blue", hjust=-4) +
      #Add reference line with a label.
       geom_hline(aes(yintercept = mean(value), colour="black"))
      # geom_text(aes( -1, mean(FemalePov18to24), label = mean(FemalePov18to24), vjust = -.5, hjust = -.25), color="red")
    
  })
  
  #map 
  output$barchartMap1 <- renderLeaflet({
    
    states <- geojsonio::geojson_read("tempgeo.json", what = "sp")
    
    bins <- c(.1, .2, .3, .4, .5, .6, .8, .9, 1, Inf)
    map_d <- mapDataQuery()
    pal <- colorBin("YlOrRd", domain = map_d$PopulationPovertyPercent, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong>
      <br/>%g State Poverty Percent
      <br/>%g White Poverty Percent
      <br/>%g Black Poverty Percent
      <br/>%g Latino/Hispanic Poverty Percent",
      states$name,  map_d$PopulationPovertyPercent, map_d$WhitePovertyPercent,map_d$BlackPovertyPercent,map_d$LatinoPovertyPercent
    ) %>% lapply(htmltools::HTML)
    m <- leaflet(states) %>%
    setView(lng = -98.35, lat = 39.5, zoom = 4) %>% 
    addTiles() %>%
      addPolygons(
        fillColor = ~pal(map_d$BlackPovertyPercent),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
      bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~map_d$BlackPovertyPercent, opacity = 0.7, title = NULL,
              position = "topright")
    
  })
  # End Barchart Tab ___________________________________________________________

  # Begin Wealth Tab ------------------------------------------------------------------
  dfwe <- eventReactive(input$click6, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edv-final-project", type="sql",
        query="select i.StateName as State, i.WhiteAbove200 as White, i.BlackAbove200 as Black, i.HispanicLatinoAbove200 as HispanicLatino from IncomeAbove200 i"
      ) # %>% View()
    }
    
  })
  output$wealthData <- renderDataTable({DT::datatable(dfwe(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  output$wealthPlot <- renderPlotly({
    dat_m <- dfwe()[, c("State","White","Black","HispanicLatino")]
    dat_m <- gather(dat_m, White, Black, HispanicLatino, key='Ethnicity', value='above200',convert = T)
    # dat_m$States <- with(dat_m, reorder(State,State,function(x)-length(x)))
    # st_table <- table(dat_m$State)
    # st_levels <- names(st_table)[order(st_table)]
    # dat_m$States <- factor(dat_m$State, levels = st_levels)
    # x <- transform(dat_m, variable=reorder(variable, -value) ) 
    # print(dat_m)
    # ggplot(dat_m, aes(reorder(States,above200), above200)) + geom_point()
    p <- ggplot(dat_m) + 
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_point(aes(x=reorder(State,above200), y=above200, colour = Ethnicity), size=2, alpha=0.5) +
      labs(x="States",y="Population above $200k")
    ggplotly(p)
  })
  # End Scatter Plots Tab ___________________________________________________________
  
  
  })