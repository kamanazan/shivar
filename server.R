library(shiny)
library(ggplot2)
library(vars)
library(tseries)
library(forecast)

#data_sumber <- read.csv("canada.csv")

proses_data <- function(dataset) {
  adf_asli <- adf.test(dataset)
  p.orig <- adf_asli$p.value
  
  # Transformasi
  lambda <- BoxCox.lambda(dataset)
  data.trans <- BoxCox(dataset, lambda)
  adf_trans <- adf.test(data.trans)
  p.trans <- adf_trans$p.value
  
  # Differencing
  diffd <- list()
  
  p.diff = 1
  data.diff = data.trans
  for (i in 1:3) {
    data.diff <- diff(data.diff, lag = 1, d = 1)
    diffd[[i]] <- data.diff
    data.diff <- diffd[[i]]
    
  }
  adf_diff1 <- adf.test(diffd[[2]])
  p.diff1 <- adf_diff1$p.value
  adf_diff2 <- adf.test(diffd[[2]])
  p.diff2 <- adf_diff2$p.value
  adf_diff3 <- adf.test(diffd[[3]])
  p.diff3 <- adf_diff3$p.value
  
  if (p.diff1 < 0.05){
    adf_diff <- adf_diff1
    res.diff <- diffd[[1]]
  }else if (p.diff2 < 0.05){
    adf_diff <- adf_diff2
    res.diff <- diffd[[2]]
  }else if (p.diff3 < 0.05){
    adf_diff <- adf_diff3
    res.diff <- diffd[[3]]
  }
  
  return(list(d.trans = data.trans, d.diff = res.diff, 
              adf.trans=adf_trans, adf.diff=adf_diff, adf.asli=adf_asli,
              df1=diffd[[1]], df2=diffd[[2]], df3=diffd[[3]]))
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
    data_proses <- vector('list')
    # Kolom pertama dianggap variable t (tidak dianalisis)
    # TODO: buat pilihan variable yang tidak akan dianalisis
    for (col in 2:num_of_col) {
      hasil <- proses_data(datanya[,col])
      
      data_proses[[nama_kolom[col]]] <-
        list(d.diff = hasil$d.diff, d.trans = hasil$d.trans, 
             adf.trans=hasil$adf.trans, adf.diff=hasil$adf.diff, adf.asli=hasil$adf.asli,
             df1=hasil$df1, df2=hasil$df2, df3=hasil$df3)
      
    }
    
    return(data_proses)
  })
  
  output$pilih_kolom <- renderUI({
    if (length(data_sumber()) > 0){
      kolom <- names(ambil_data())
      selectInput(
        "var_column",
        label = "Kolom yang diamati",
        choices = kolom,
        selected = kolom[2]
      )
    }
  })
  
  ringkasan_adf <- reactive({
    anu <- ambil_data()
    kolom <- anu[[input$var_column]]
    dt <- kolom$adf.trans
    df <- kolom$adf.diff
    # Membuat table summary yang baru
    num_of_col = 2
    num_of_row = 3
    tot_elm = num_of_row * num_of_col
    tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    
    colnames(tbl) <- c('Transformasi', 'Differencing')
    rownames(tbl) <-
      c("Dickey-Fuller", "Lag Order", "Nilai P")
    
    tbl[1,1] <- dt$statistic[[1]]
    tbl[2,1] <- dt$parameter[[1]]
    tbl[3,1] <- dt$p.value[[1]]
    tbl[1,2] <- df$statistic[[1]]
    tbl[2,2] <- df$parameter[[1]]
    tbl[3,2] <- df$p.value[[1]]
    
    return(as.table(tbl))
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

          if (adf_test$p.value[[1]] < 0.05)
          {
            new_sr[row,col] = paste("Stationer: Ya  ")
          }
          else
          {
            new_sr[row,col] = paste("Stationer: Tidak  ")
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
  
  get_estimation_data <- reactive({
    
    dt <- data_sumber()
    anu <- ambil_data()
    # Membuat table summary yang baru
    
    num_of_col = length(names(anu))
    num_of_row = length(dt[,1])
    tot_elm = num_of_row * num_of_col
    tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    colnames(tbl) <- names(anu)
    
    for (col in 1:num_of_col) {
      nama_kolom <- colnames(tbl)[col]
      p_asli <- anu[[col]]$adf.asli$p.value
      p_diff <- anu[[col]]$adf.diff$p.value
      p_trans <- anu[[col]]$adf.trans$p.value
      
      if (p_asli < 0.05){
        datanya <- dt[[col]]
      }else if(p_trans < 0.05){
        datanya <- anu[[col]]$d.trans
      }else{
        datanya <- anu[[col]]$d.diff
      }
      
      ln <- length(datanya)
      if (ln < num_of_row){
        selisih <- num_of_row - ln
        for (i in 1:selisih)
          datanya <- c(datanya, NA)
      }
      tbl[,col] <- datanya
    }
    # Jadi gini karena data yang dipakai adalah data yang terakhir stationer
    # ada kemungkinan jumlah data setiap kolom beda-beda, jadinya jumlah data
    # disamaratakan berdasarkan hasil differncing 3
    row_used <- num_of_row - 3
    return(tbl[1:row_used,])
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
  
  output$var_select <- renderDataTable({
    vselect <- var_process()
    if (!is.null(vselect))
      vselect$criteria
  })
  
  output$data_table <- renderDataTable({
    if (length(data_sumber()) > 0)
      data_sumber()
  })
  
  output$data_summary <- renderTable({
    if (length(data_sumber()) > 0)
      get_summary()
  })
  
  output$asli_ts <- renderPlot({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      plot.ts(dt[,input$var_column], main = paste("Diagram Fit untuk ",input$var_column), ylab =
                "value")
    }
  })
  
  output$asli_acf <- renderPlot({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      resid <- acf(dt[,input$var_column])
      plot(resid, main = paste("ACF Residual untuk ",input$var_column))
    }
  })
  
  output$asli_pacf <- renderPlot({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      resid <- pacf(dt[,input$var_column],plot = FALSE)
      plot(resid, main = paste("PACF Residual untuk ",input$var_column))
    }
    
  })
  
  
  
  output$data_transformasi <- renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      # Membuat table summary yang baru
      
      num_of_col = length(names(anu))
      num_of_row = length(dt[,1])
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$d.trans
        tbl[,col] <- datanya
      }
      
      tbl
    }
  })
  
  output$data_differencing1 <- renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      # Membuat table summary yang baru
      
      num_of_col = length(names(anu))
      num_of_row = length(anu[[1]]$df1)
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$df1
        tbl[,col] <- datanya
      }
      
      tbl
    }
  })
  
  output$data_differencing2 <- renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      # Membuat table summary yang baru
      
      num_of_col = length(names(anu))
      num_of_row = length(anu[[1]]$df2)
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$df2
        tbl[,col] <- datanya
      }
      
      tbl
    }
  })
  
  output$data_differencing3 <- renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      # Membuat table summary yang baru
      
      num_of_col = length(names(anu))
      num_of_row = length(anu[[1]]$df3)
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$df3
        tbl[,col] <- datanya
      }
      
      tbl
    }
  })
  
  output$hasil_adf <- renderTable({
    if (length(data_sumber()) > 0)
      ringkasan_adf()
  })
  
  output$id_ts <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column]]
      dt <- kolom$d.trans
      df <- kolom$d.diff
      par(mfrow = c(1,2))
      plot.ts(
        dt, main = paste("Plot untuk ",input$var_column, "setelah Transformasi"), ylab =
          "value"
      )
      plot.ts(
        df, main = paste("Plot untuk ",input$var_column, "setelah differencing"), ylab =
          "value"
      )
    }
  })
  
  output$id_acf <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column]]
      dt <- acf(kolom$d.trans)
      df <- acf(kolom$d.diff)
      par(mfrow = c(1,2))
      plot(
        dt, main = paste("ACF untuk ",input$var_column, "setelah Transformasi"), ylab =
          "value"
      )
      plot(
        df, main = paste("ACF untuk ",input$var_column, "setelah differencing"), ylab =
          "value"
      )
    }
  })
  
  output$id_pacf <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column]]
      dt <- pacf(kolom$d.trans)
      df <- pacf(kolom$d.diff)
      par(mfrow = c(1,2))
      plot(
        dt, main = paste("PACF untuk ",input$var_column, "setelah Transformasi"), ylab =
          "value"
      )
      plot(
        df, main = paste("PACF untuk ",input$var_column, "setelah differencing"), ylab =
          "value"
      )
    }
  })
  
  output$estimasi_hasil <- renderPrint({
    if (length(data_sumber()) > 0) {
      dt <- get_estimation_data()
      VAR(dt,p=input$estimasi_p.val,type=input$estimasi_type)
    }
  })
  
})
