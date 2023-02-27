
# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ECharts2Shiny)
library(plotly)



monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

Sales_Summarized<-read.csv("D:\\Shiny_Radius\\Datasets\\Sales_Summarized.csv")

Sales_Summarized$Date=ymd(Sales_Summarized$Date)

# create the server functions for the dashboard  
shinyServer(function(session,input, output) { 
  
  rv = reactiveValues()
  rv$Sales_Summarized=Sales_Summarized
  
  
  


  
  observe({
    
    rv$Sales_Summarized <- Sales_Summarized[Sales_Summarized$Brand == input$Brand_P1,]

    updateSliderInput(session,"DateFilter_P1",min = min(rv$Sales_Summarized$Date),
                      max = max(rv$Sales_Summarized$Date),
                      value=c(min(rv$Sales_Summarized$Date),max(rv$Sales_Summarized$Date)),timeFormat="%b %Y")

    
    
    
    })
  
  
  output$salesplot <- renderPlotly({
    
    DF3<-rv$Sales_Summarized%>%dplyr::filter(Date>=input$DateFilter_P1[1])%>%dplyr::filter(Date<=input$DateFilter_P1[2])%>%dplyr::arrange(Date)
    DF3$Monthly_Growth<-0
    for(i in seq(2,nrow(DF3))){
      DF3$Monthly_Growth<-((DF3$Monthly_Growth[i]-DF3$Monthly_Growth[i-1])/DF3$Monthly_Growth[i-1])*100
    }
    plot_ly(DF3, x = ~Date, 
            y = ~total, 
            name = 'trace 0', 
            type = 'scatter', 
            mode = 'lines')%>%
      plotly::layout( height = 325)
  })
  
  
  observeEvent(input$button, {
    updateTabItems(session, "tabs", "Marketing_Analysis")

  })
  
  
  })
