library(shiny)
library(ggplot2)
library(vars)
library(tseries)
library(forecast)

#data_sumber <- read.csv("canada.csv")

proses_data <- function(dataset) {
  p.orig <- adf.test(dataset)$p.value
  
  # Transformasi
  lambda <- BoxCox.lambda(dataset)
  data.trans <- BoxCox(dataset, lambda)
  adf_trans <- adf.test(data.trans)
  p.trans <- adf_trans$p.value
  
  # Differencing
  data.diff <- diff(dataset, lag = 1, d = 1)
  adf_diff <- adf.test(data.diff)
  p.diff <- adf_diff$p.value
  
  #both
  data.both <- diff(data.trans, lag = 1, d = 1)
  adf_both <- adf.test(data.both) 
  p.both <- adf_both$p.value
  
  if (p.orig < 0.1) {
    dt <- dataset
    met <- 'orig'
    adf <- NULL
  }else if (p.trans < 0.1) {
    dt <- data.trans
    met <- 'trans'
    adf <- adf_trans
  }else if (p.diff < 0.1) {
    dt <- data.diff
    met <- 'diff'
    adf <- adf_diff
  }else{
    dt <- data.both
    met <- 'both'
    adf <- adf_both
  }
  return(list(dt=dt, met=met, adf=adf))  
}

shinyServer(function(input, output) {
  data_sumber <- reactive({
    if (length(input$sumber_data$name) > 0) {
      dt <- read.csv(input$sumber_data$datapath)
    }
    else
      dt <- list()
    return(dt)
  })
  
  ambil_data <- reactive({
    datanya <- data_sumber()
    nama_kolom <- colnames(datanya)
    num_of_col <- length(nama_kolom)
    data_proses <- list() # pake nama yang lebih jelalah
    
    for (col in 1:num_of_col) {
      hasil_proses <- proses_data(datanya[,col])
      
      data_proses[nama_kolom[col]] <- list(dt=hasil_proses$dt, met=hasil_proses$met, adf=hasil_proses$adf)
    }
    print(data_proses)
    return(data_proses)
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
    new_sr <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    colnames(new_sr) <- colnames(sr)
    
    for (col in 1:num_of_col)
    {
      for (row in 1:num_of_row)
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
    dt <- ambil_data()
    kolom <- dt[,input$var]
    # Membuat table summary yang baru
    num_of_col = 1
    num_of_row = 4
    tot_elm = num_of_row * num_of_col
    tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    
    colnames(tbl) <- colnames(dt)
    rownames(tbl) <-
      c("Dickey-Fuller", "Lag Order", "Nilai P", "Kesimpulan")
    
    adf_test <- adf.test(dt[,col])
    tbl["Dickey-Fuller",col] <- dt$statistic[[1]]
    tbl["Lag Order",col] <- adf_test$parameter[[1]]
    tbl["Nilai P",col] <- adf_test$p.value[[1]]
    tbl["Kesimpulan",col] <- adf_test$alternative
    #tbl["Dickey-Fuller",col] <- c(stat, param, p_val, is_sti)
      
    
    
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
  
  output$asli_ts <- renderPlot({
    if (length(data_sumber()) > 0)
      dt <- data_sumber()
    plot.ts(dt[,input$var_column], main = paste("Diagram Fit untuk ",input$var_column), ylab =
              "value")
  })
  
  output$asli_acf <- renderPlot({
    if (length(data_sumber()) > 0)
      dt <- data_sumber()
    resid <- acf(dt[,input$var_column])
    plot(resid, main = paste("ACF Residual untuk ",input$var_column))
  })
  
  output$asli_pacf <- renderPlot({
    if (length(data_sumber()) > 0)
      dt <- data_sumber()
    resid <- pacf(dt[,input$var_column],plot=FALSE)
    plot(resid, main = paste("PACF Residual untuk ",input$var_column))
  })
  
  output$identifikasi <- renderTable({
    if (length(data_sumber()) > 0){
     anu <- ambil_data()
    kolom <- anu[input$var_column]
    print(kolom)
    # Membuat table summary yang baru
    num_of_col = 1
    num_of_row = 4
    tot_elm = num_of_row * num_of_col
    tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    
    colnames(tbl) <- colnames(dt)
    rownames(tbl) <-
      c("Dickey-Fuller", "Lag Order", "Nilai P", "Kesimpulan")
    
    
    tbl["Dickey-Fuller",col] <- kolom$adf$statistic[[1]]
    tbl["Lag Order",col] <- kolom$adf$parameter[[1]]
    tbl["Nilai P",col] <- kolom$adf$p.value[[1]]
    tbl["Kesimpulan",col] <- kolom$met
    }
  })
})
