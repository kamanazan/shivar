library(shiny)
library(ggplot2)
library(vars)
library(tseries)

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
  
  get_summary <- reactive({
    dt <- data_sumber()
    sr <- summary(dt)
    # Membuat table summary yang baru
    num_of_col = length(colnames(sr))
    num_of_row = 7
    tot_elm = num_of_row * num_of_col
    new_sr <- array(1:tot_elm, dim=c(num_of_row, num_of_col))
    colnames(new_sr) <- colnames(sr)
    
    for(col in 1: num_of_col) 
    {
      for(row in 1:num_of_row)
      {
        if (row == 7) 
        {
          adf_test <- adf.test(dt[,col])
          if (adf_test$alternative == "stationary")
          {
            new_sr[row,col] = paste("Stationer: Ya  ")
          } 
          else
          {
            new_sr[row,col] == paste("Stationer: Tidak  ")
          }
        }
        else
        {
          new_sr[row,col] = sr[row,col]
        }
        
      } 
    }
    tbl <- as.table(new_sr)
    row.names(tbl) <- NULL
    return(tbl)
  })
  
  identifikasi <- reactive({
    dt <- data_sumber()
    
    # Membuat table summary yang baru
    num_of_col = length(colnames(dt))
    num_of_row = 4
    tot_elm = num_of_row * num_of_col
    tbl <- array(1:tot_elm, dim=c(num_of_row, num_of_col))
    
    colnames(tbl) <- colnames(dt)
    rownames(tbl) <- c("Dickey-Fuller", "Lag Order", "Nilai P", "Kesimpulan")
    for(col in 1: num_of_col) 
    {
      adf_test <- adf.test(dt[,col])
      tbl["Dickey-Fuller",col] <- adf_test$statistic[[1]]
      tbl["Lag Order",col] <- adf_test$parameter[[1]]
      tbl["Nilai P",col] <- adf_test$p.value[[1]]
      tbl["Kesimpulan",col] <- adf_test$alternative
      #tbl["Dickey-Fuller",col] <- c(stat, param, p_val, is_sti)
      
    }
    
    return(as.table(tbl))
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
  
  var_residual <- reactive({
    va <- var_analysis()
    rds <- residuals(va)
    return(rds)
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
      get_summary()
  })
  
  output$var_fit <- renderPlot({
    vselect <- var_process()
    if (!is.null(vselect)) {
      va <- var_analysis()
      resids <- var_residual()
      plot.ts(
        resids[,input$var_column], main = paste("Diagram Fit untuk ",input$var_column), ylab =
          "value"
      )
    }
  })
  
  output$acf_residual <- renderPlot({
    vselect <- var_process()
    if (!is.null(vselect)) {
      va <- var_analysis()
      resids <- var_residual()
      acf(
        resids[,input$var_column], main = paste("ACF Residual untuk ",input$var_column), lag.max =
          12
      )
    }
  })
  
  output$pacf_residual <- renderPlot({
    vselect <- var_process()
    if (!is.null(vselect)) {
      va <- var_analysis()
      resids <- var_residual()
      pacf(
        resids[,input$var_column], main = paste("PACF Residual untuk ",input$var_column), lag.max =
          12
      )
    }
  })
  
  output$identifikasi <- renderTable({
    if (length(data_sumber()) > 0)
      identifikasi()
  })
})