library(shiny)
library(DT)
library(vars)
library(tseries)
library(forecast)
library(tools)
library(xlsx)

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
  adf_diff1 <- adf.test(diffd[[1]])
  p.diff1 <- adf_diff1$p.value
  adf_diff2 <- adf.test(diffd[[2]])
  p.diff2 <- adf_diff2$p.value
  adf_diff3 <- adf.test(diffd[[3]])
  p.diff3 <- adf_diff3$p.value
  
  if (p.diff1 < 0.05) {
    adf_diff <- adf_diff1
    res.diff <- diffd[[1]]
  }else if (p.diff2 < 0.05) {
    adf_diff <- adf_diff2
    res.diff <- diffd[[2]]
  }else if (p.diff3 < 0.05) {
    adf_diff <- adf_diff3
    res.diff <- diffd[[3]]
  }
  
  return(
    list(
      d.trans = data.trans, d.diff = res.diff, d.diff1 = diffd[[1]], d.diff2 = diffd[[2]], d.diff3 = diffd[[3]],
      adf.trans = adf_trans, adf.diff = adf_diff, adf.asli = adf_asli,
      adf.diff1 = adf_diff1, adf.diff2 = adf_diff2, adf.diff3 = adf_diff3,
      df1 = diffd[[1]], df2 = diffd[[2]], df3 = diffd[[3]]
    )
  )
}

shinyServer(function(input, output) {
  data_sumber <- reactive({
    if (length(input$sumber_data$name) > 0) {
      ext <- file_ext(input$sumber_data$name)
      if (ext == 'csv'){
        dt <- read.csv(input$sumber_data$datapath)
      }else if ((ext == 'xls') | (ext == 'xlsx')){
        dt <- read.xlsx(input$sumber_data$datapath, sheetIndex = 1)
      }
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
        list(
          d.diff = hasil$d.diff, d.trans = hasil$d.trans,d.diff1 = hasil$d.diff1, d.diff2 = hasil$d.diff2, d.diff3 = hasil$d.diff3,
          adf.trans = hasil$adf.trans, adf.diff = hasil$adf.diff, adf.asli =
            hasil$adf.asli, adf.diff1 = hasil$adf.diff1, adf.diff2 = hasil$adf.diff2, adf.diff3 = hasil$adf.diff3,
          df1 = hasil$df1, df2 = hasil$df2, df3 = hasil$df3
        )
      
    }
    
    return(data_proses)
  })
  
  output$pilih_kolom <- renderUI({
    if (length(data_sumber()) > 0) {
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
    #df <- kolom$adf.diff      
    if (input$adf_diff_level == 1){
      df <- kolom$adf.diff1
    } else if (input$adf_diff_level == 2){
      df <- kolom$adf.diff2
    } else if (input$adf_diff_level == 3){
      df <- kolom$adf.diff3
    }
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
    num_of_col = length(colnames(dt))
    num_of_row = 7
    tot_elm = num_of_row * num_of_col
    new_sr <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    colnames(new_sr) <- colnames(dt)
    
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
    colnames(tbl) <- colnames(sr)
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
      
      if (p_asli < 0.05) {
        datanya <- dt[[col]]
      }else if (p_trans < 0.05) {
        datanya <- anu[[col]]$d.trans
      }else{
        datanya <- anu[[col]]$d.diff
      }
      
      ln <- length(datanya)
      if (ln < num_of_row) {
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
  
  var_analysis <- reactive({
    dt <- get_estimation_data()
    va <-
      VAR(dt,p = input$estimasi_p.val,type = input$estimasi_type)
    return(va)
  })
  
  var_summary <- reactive({
    va <- var_analysis()
    va_res <- va$varresult
    va_summ <- summary(va)$varresult
    
    #ambil sample jumlah kolom dan jumlah baris
    nama_kolom <- names(va_res)
    nama_baris <- names(va_res[[1]]$coefficients)
    
    #membuat tabel
    num_of_col = length(nama_kolom)
    num_of_row = length(nama_baris)
    tot_elm = num_of_row * num_of_col
    tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
    colnames(tbl) <- nama_kolom
    rownames(tbl) <- nama_baris
    
    for (kol in nama_kolom) {
      coef_var <- va_res[[kol]]$coefficients
      coef_summary <- va_summ[[kol]]$coefficients
      for (bar in nama_baris) {
        nilai_analisis <- coef_var[[bar]]
        nilai_error <- coef_summary[bar, "Std. Error"]
        nilai_pt <- coef_summary[bar, "Pr(>|t|)"]
        # penentuan signif. codes
        if (nilai_pt< 0.001){
          bintang <- '***'
        } else if ((nilai_pt >= 0.001) & (nilai_pt < 0.01)){
          bintang <- '**'
        } else if ((nilai_pt >= 0.01) & (nilai_pt < 0.05)){
          bintang <- '*'
        } else if ((nilai_pt >= 0.05) & (nilai_pt < 0.1)){
          bintang <- '.'
        } else bintang <- ''
          
        tbl[bar,kol] <-
          paste(nilai_analisis, '<br>',nilai_error, '<br>',nilai_pt,'<strong>',bintang,"</strong>")
      }
    }
    
    return(tbl)
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
  
  output$data_table <- DT::renderDataTable({
    if (length(data_sumber()) > 0)
      data_sumber()
  }, option = list(searching = FALSE,
                   rownames = FALSE))
  
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
  
  output$data_transformasi <- DT::renderDataTable({
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
  }, option = list(searching = FALSE,
                   rownames = FALSE))
  
  output$diff_summary <- renderTable({
    if (length(data_sumber()) > 0){
      get_summary()
    }
     
  }, option = list(searching = FALSE,
                   paging = FALSE,
                   rownames = FALSE))
  
  output$data_differencing1 <- DT::renderDataTable({
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
  }, option = list(searching = FALSE,
                   rownames = FALSE))
  
  output$data_differencing2 <- DT::renderDataTable({
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
  }, option = list(searching = FALSE,
                   rownames = FALSE))
  
  output$data_differencing3 <- DT::renderDataTable({
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
  }, option = list(searching = FALSE,
                   rownames = FALSE))
  
  output$hasil_adf <- renderTable({
    if (length(data_sumber()) > 0)
      ringkasan_adf()
  })
  
  output$id_ts <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column]]
      dt <- kolom$d.trans
      #df <- kolom$d.diff
      if (input$adf_diff_level == 1){
        df <- kolom$d.diff1
      } else if (input$adf_diff_level == 2){
        df <- kolom$d.diff2
      } else if (input$adf_diff_level == 3){
        df <- kolom$d.diff3
      }
      par(mfrow = c(1,2))
      plot.ts(
        dt, main = paste("Plot untuk ",input$var_column, "setelah Transformasi"),
        ylab = "value"
      )
      plot.ts(
        df, main = paste("Plot untuk ",input$var_column, "setelah differencing"),
        ylab = "value"
      )
    }
  })
  
  output$id_acf <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column]]
      dt <- acf(kolom$d.trans)
      #df <- kolom$d.diff
      if (input$adf_diff_level == 1){
        df <- acf(kolom$d.diff1)
      } else if (input$adf_diff_level == 2){
        df <- acf(kolom$d.diff2)
      } else if (input$adf_diff_level == 3){
        df <- acf(kolom$d.diff3)
      }
      par(mfrow = c(1,2))
      plot(
        dt, main = paste("ACF untuk ",input$var_column, "setelah Transformasi"),
        ylab = "value"
      )
      plot(
        df, main = paste("ACF untuk ",input$var_column, "setelah differencing"),
        ylab = "value"
      )
    }
  })
  
  output$id_pacf <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column]]
      dt <- pacf(kolom$d.trans)
      #df <- kolom$d.diff
      if (input$adf_diff_level == 1){
        df <- pacf(kolom$d.diff1)
      } else if (input$adf_diff_level == 2){
        df <- pacf(kolom$d.diff2)
      } else if (input$adf_diff_level == 3){
        df <- pacf(kolom$d.diff3)
      }
      par(mfrow = c(1,2))
      plot(
        dt, main = paste("PACF untuk ",input$var_column, "setelah Transformasi"),
        ylab = "value"
      )
      plot(
        df, main = paste("PACF untuk ",input$var_column, "setelah differencing"),
        ylab = "value"
      )
    }
  })
  
  output$estimasi_hasil <- renderPrint({
    if (length(data_sumber()) > 0) {
      var_analysis()
    }
  })
  
  output$estimasi_kesimpulan <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      var_summary()
    }
  }, escape = FALSE,
  options = list(
    paging = FALSE,
    processing = FALSE,
    searching = FALSE
  ))
  
  output$diagnostic_serial <- renderPrint({
    if (length(data_sumber()) > 0) {
      va <- var_analysis()
      serial.test(va, lags.pt = 16)
    }
  })
  output$diagnostic_normal <- renderPrint({
    if (length(data_sumber()) > 0) {
      va <- var_analysis()
      normality.test(va)
    }
  })
  output$fcst_tbl <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      va <- var_analysis()
      pr <- predict(va, n.ahead = input$fcst.time)
      tbl <- pr$fcst[[input$var_column]]
      rownames(tbl) <- as.character(c(1:input$fcst.time))
      tbl
    }
  }, escape = FALSE,
  options = list(
    paging = FALSE,
    processing = FALSE,
    searching = FALSE
  ))
  
})
