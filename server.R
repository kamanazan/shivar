library(shiny)
library(ggplot2)
library(vars)

#data_sumber <- read.csv("canada.csv")

shinyServer(function(input, output) {
  data_sumber <- reactive({
    if (length(input$sumber_data$name) > 0) {
      dt <- read.csv(input$sumber_data$datapath)
    }
    else
      dt <- list()
    return(dt)
  })
  
  output$pilih_kolom <- renderUI({
    kolom <- colnames(data_sumber())
    selectInput(
      "var_column",
      label = "Kolom yang diteliti",
      choices = kolom,
      selected = kolom[2]
    )
  })
  
  var_process <- reactive({
    datanya <- data_sumber()
    if (length(datanya) > 0)
      vr <-
        VARselect(data_sumber(),lag.max = input$lag_max)
    else
      vr <- NULL
  })
  
  var_analysis <- reactive({
    vselect <- var_process()
    p_val <- vselect$selection[['SC(n)']]
    p1ct <- VAR(data_sumber(), p = p_val)
    return(p1ct)
  })
  
  arch_test <- reactive({
    va <- var_analysis()
    arch <- arch.test(va, lags.multi = 5)
  })
  
  stable_test <- reactive({
    return(stability(var_analysis()))
  })
  
  output$var_select <- renderTable({
    vselect <- var_process()
    if (!is.null(vselect))
      vselect$criteria
  })
  
  output$data_table <- renderTable({
    if (length(data_sumber()) > 0)
      data_sumber()
  })
  
  output$data_summary <- renderTable({
    if (length(data_sumber()) > 0)
      summary(data_sumber())
  })
  
  output$var_result <- renderPlot({
    vselect <- var_process()
    if (!is.null(vselect)) {
      va <- var_analysis()
      plot(
        va, names = input$var_column, nc = 2
      )
    }
  })
  
  output$residual <- renderPlot({
    plot(
      arch_test(), names = input$var_column, nc = 2
    )
  })
  
  output$stability <- renderPlot({
    plot(stable_test(), nc = 2)
  })
})