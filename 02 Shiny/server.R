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

online0 = TRUE

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
if(online0) {
  counties = query(
    data.world(propsfile = "www/.data.world"),
    dataset="lordlemon/s-17-edvproject-6", type="sql",
    query="select County as D from Education"
  ) # %>% View()
} else {
}
counties_list <- as.list(counties$D, counties$D)
counties_list <- append(list("All" = "All"), counties_list)
#region_list5 <- region_list


tdf = query(
  data.world(propsfile = "www/.data.world"),
  dataset="lordlemon/s-17-edvproject-6", type="sql",
  query="select p.County as County, p.MalePov18to24 as MalePov18to24, e.MaleUnderGrad as MaleUnderGrad
  from Education e inner join Poverty p on e.County = p.County
  where FemalePov18to24 > 0 and MalePov18to24 > 0
  group by p.County
  ", queryParameters = "p.County"
  
) # %>% View()
tdf = tdf[ grepl("County", tdf$County) , ]


# 



############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$counties2 <- renderUI({selectInput("selectedCounties", "Choose Counties:", counties_list, multiple = TRUE, selected='All') })
  
  
  # Begin Barchart Tab ------------------------------------------------------------------
  dfbc1 <- eventReactive(input$click2, {
    if(input$selectedCounties == 'All') counties_list <- input$selectedCounties
    else counties_list <- append(list("Skip" = "Skip"), input$selectedCounties)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="lordlemon/s-17-edvproject-6", type="sql",
        query="select p.County as County, p.FemalePov18to24 as FemalePov18to24, e.FemaleUnderGrad as FemaleUnderGrad, p.MalePov18to24 as MalePov18to24, e.MaleUnderGrad as MaleUnderGrad
        from Education e inner join Poverty p on e.County = p.County
        where MalePov18to24 >0 and MaleUnderGrad >0
        group by p.County
        ",
        queryParameters = counties_list
      ) #%>% View()
    }
    else {
    }
    # 
    
    tdf = tdf[ grepl("County", tdf$County) , ]
  })
  
  
  
  
  
  output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartData3 <- renderDataTable({DT::datatable(tdf,
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartPlot1 <- renderPlot({ggplot(dfbc1(), aes(x=County, y=FemalePov18to24)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity")  +
      #facet_wrap(~County, ncol=4) + 
      coord_flip() +
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=County, y=FemalePov18to24, label=round(FemalePov18to24)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=County, y=FemalePov18to24, label=round(FemalePov18to24- FemaleUnderGrad)),colour="blue", hjust=-4) +
      #Add reference line with a label.
      geom_hline(aes(yintercept = round(mean(FemalePov18to24)), color="red"))+
      geom_text(aes( -1, mean(FemalePov18to24), label = mean(FemalePov18to24), vjust = -.5, hjust = -.25), color="red")
    
  })
  
  output$barchartPlot2 <- renderPlot({ggplot(dfbc1(), aes(x=County, y=MalePov18to24)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity")  +
      #facet_wrap(~County, ncol=4) + 
      coord_flip() +
      # Add MalePov18to24, and (MalePov18to24 - MaleUnderGrad) label.
      geom_text(mapping=aes(x=County, y=MalePov18to24, label=round(MalePov18to24)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=County, y=MalePov18to24, label=round(MalePov18to24- MaleUnderGrad)),colour="blue", hjust=-4) +
      #Add reference line with a label.
      geom_hline(aes(yintercept = round(mean(MalePov18to24)), color="red"))+
      geom_text(aes( -1, mean(MalePov18to24), label = mean(MalePov18to24), vjust = -.5, hjust = -.25), color="red")
    
  })
  
  output$barchartPlot3 <- renderPlotly({ggplot(tdf, aes(x=County, y=MalePov18to24)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity")  +
      #facet_wrap(~County, ncol=4) + 
      coord_flip() +
      # # Add sum_sales, and (sum_sales - window_avg_sales) label.
      # geom_text(mapping=aes(x=County, y=FemalePov18to24, label=round(FemalePov18to24)),colour="black", hjust=-.5) +
      # geom_text(mapping=aes(x=County, y=FemalePov18to24, label=round(FemalePov18to24- FemaleUnderGrad)),colour="blue", hjust=-4) +
      #Add reference line with a label.
      geom_hline(aes(yintercept = round(mean(MalePov18to24)), color="red"))+
      geom_text(aes(1 , mean(MalePov18to24), label = mean(MalePov18to24), vjust = 0, hjust = 10), color="red")
    
  })
  
  
  # End Barchart Tab ___________________________________________________________
  
  })