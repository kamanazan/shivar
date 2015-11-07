library(shiny)
library(ggplot2)
#library(data.table)
#library(scales)
#library(stockPortfolio)
#library(plyr)
#library(RQuantLib)
#library(quantmod)
#library(e1071)
#library(gridExtra)
#library(lubridate)
#library(magrittr)
library(vars)

data_sumber <- read.csv("canada.csv")

shinyServer(
  function(input, output) {
    
    var_process <- reactive({
      vr <- VARselect(data_sumber,lag.max=input$lag_max, type=input$var_type)
      return(vr)
    })
    
    output$var_select <- renderTable({ 
      vselect <- var_process()
      vselect$criteria
    })
    
    output$data_table <- renderTable({
      data_sumber
    })
    
    output$var_result <- renderPlot({
      vselect <- var_process()
      p_val <- vselect$selection[['SC(n)']]
      p1ct <- VAR(data_sumber[c("e","prod","rw","U")], p=p_val, type=input$var_type)
      plot(p1ct, names="e")
    })
  }
)