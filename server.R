library(shiny)
library(DT)
library(vars)
library(tseries)
library(forecast)
library(tools)
library(xlsx)

p.val <- 2
plot_settings <- par(ps = 14, cex = 1, 
                     cex.main = 1, 
                     lwd = 2)

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
  
  # dibuat anti-nya
  anti.diff <- diffinv(res.diff, lag=1,differences=1)
  anti.trans <- InvBoxCox(data.trans,lambda=lambda)
  
  return(
    list(
      d.trans = data.trans, d.diff = res.diff, d.diff1 = diffd[[1]], d.diff2 = diffd[[2]], d.diff3 = diffd[[3]],
      adf.trans = adf_trans, adf.diff = adf_diff, adf.asli = adf_asli,
      adf.diff1 = adf_diff1, adf.diff2 = adf_diff2, adf.diff3 = adf_diff3,
      df1 = diffd[[1]], df2 = diffd[[2]], df3 = diffd[[3]], lambda = round(lambda, digits=3),
      anti.diff = anti.diff, anti.trans = anti.trans
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
          df1 = hasil$df1, df2 = hasil$df2, df3 = hasil$df3, lambda = hasil$lambda, anti.diff = hasil$anti.diff, anti.trans = hasil$anti.trans
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
  
  output$pilih_kolom2 <- renderUI({
    if (length(data_sumber()) > 0) {
      kolom <- names(ambil_data())
      selectInput(
        "var_column2",
        label = "Kolom yang diamati",
        choices = kolom,
        selected = kolom[2]
      )
    }
  })
  
  output$pilih_kolom3 <- renderUI({
    if (length(data_sumber()) > 0) {
      kolom <- names(ambil_data())
      selectInput(
        "var_column3",
        label = "Kolom yang diamati",
        choices = kolom,
        selected = kolom[2]
      )
    }
  })
  
  ringkasan_adf <- reactive({
    anu <- ambil_data()
    kolom <- anu[[input$var_column2]]
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
          
          if (col  == 1)
          {
            new_sr[row,col] = paste("-  ")
          }
          else if (adf_test$p.value[[1]] < 0.05)
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
  
  var_select <- reactive({
    dt <- data_sumber()
    nama_kolom <- colnames(dt)
    num_of_col <- length(nama_kolom)
    vs <-
      VARselect(dt[,2:num_of_col],lag.max = 5)
    p.val <<- min(vs$selection)
    return(vs)
  })
  
  var_analysis <- reactive({
    dt <- get_estimation_data()
    var_p <<- p.val
    var_type <<- input$estimasi_type
    va <-
      VAR(dt,p = var_p, type = var_type)
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
  
  
  get_forecasting <- reactive({
    kolom <- input$var_column3
    if (length(kolom) == 0){
      kolom <- input$var_column
    }
    va <- var_analysis()
    var_list <- names(va$varresult)
    va2 <- irf(va, impulse=kolom, response=var_list[!is.element(var_list, kolom)])
    par(plot_settings)
    plot(va2)
  })
  
  
  
  output$var_select <- renderPrint({
    if (length(data_sumber()) > 0)
      var_select()
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
      par(plot_settings)
      plot.ts(dt[,input$var_column], main = paste(" Gambar 1 : Plot Time Series ",input$var_column), ylab =
                "value", col='red', lwd = 2)
    }
  })
  
  output$asli_acf <- renderPlot({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      par(plot_settings)
      resid <- acf(dt[,input$var_column])
      plot(resid, main = paste("Gambar 2 : Plot Auto Correlation Function ",input$var_column),col = 'green', lwd = 2)
    }
  })
  
  output$asli_pacf <- renderPlot({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      par(plot_settings)
      resid <- pacf(dt[,input$var_column],plot = FALSE)
      plot(resid, main = paste("Gambar 3: Plot Partial Auto Correlation Function ",input$var_column), col = 'blue', lwd = 2)
    }
    
  })
  
  output$lambda_transformasi <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      
      num_of_col = length(names(anu))
      num_of_row = 1
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$lambda
        tbl[,col] <- datanya
      }
      
      tbl
    }
  }, option = list(searching = FALSE,
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Transformasi dengan Boxcox, ditransformasi berdasarkan nilai lambda')
  ))
  
  output$data_transformasi <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      
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
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Hasil transformasi dari data berdasarkan nilai lambda')
  ))
  
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
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Data setelah di lakukan differensing')
  )
  )
  
  output$data_differencing2 <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      
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
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Data setelah di lakukan differensing')
  )
  )
  
  output$data_differencing3 <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      
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
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Data setelah di lakukan differensing')
  )
  )
  
  output$hasil_adf <- renderTable({
    if (length(data_sumber()) > 0)
      ringkasan_adf()
  })
  
  output$id_ts <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column2]]
      dt <- kolom$d.trans
      #df <- kolom$d.diff
      if (input$adf_diff_level == 1){
        df <- kolom$d.diff1
      } else if (input$adf_diff_level == 2){
        df <- kolom$d.diff2
      } else if (input$adf_diff_level == 3){
        df <- kolom$d.diff3
      }
      par(ps = 14, cex = 1, 
          cex.main = 1, 
          lwd = 2, mfrow = c(1,2))
      plot.ts(
        dt, main = paste("Plot untuk ",input$var_column2, "setelah Transformasi"),
        ylab = "value", col = 'green'
      )
      plot.ts(
        df, main = paste("Plot untuk ",input$var_column2, "setelah differencing"),
        ylab = "value", col = 'red'
      )
    }
  })
  
  output$id_acf <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column2]]
      dt <- acf(kolom$d.trans)
      #df <- kolom$d.diff
      if (input$adf_diff_level == 1){
        df <- acf(kolom$d.diff1)
      } else if (input$adf_diff_level == 2){
        df <- acf(kolom$d.diff2)
      } else if (input$adf_diff_level == 3){
        df <- acf(kolom$d.diff3)
      }
      par(ps = 14, cex = 1, 
          cex.main = 1, 
          lwd = 2, mfrow = c(1,2))
      plot(
        dt, main = paste("ACF untuk ",input$var_column2, "setelah Transformasi"),
        ylab = "value", col = 'green'
      )
      plot(
        df, main = paste("ACF untuk ",input$var_column2, "setelah differencing"),
        ylab = "value", col = 'red'
      )
    }
  })
  
  output$id_pacf <- renderPlot({
    if (length(data_sumber()) > 0) {
      anu <- ambil_data()
      kolom <- anu[[input$var_column2]]
      dt <- pacf(kolom$d.trans)
      #df <- kolom$d.diff
      if (input$adf_diff_level == 1){
        df <- pacf(kolom$d.diff1)
      } else if (input$adf_diff_level == 2){
        df <- pacf(kolom$d.diff2)
      } else if (input$adf_diff_level == 3){
        df <- pacf(kolom$d.diff3)
      }
      par(ps = 14, cex = 1, 
          cex.main = 1, 
          lwd = 2, mfrow = c(1,2))
      plot(
        dt, main = paste("PACF untuk ",input$var_column2, "setelah Transformasi"),
        ylab = "value", col = 'green'
      )
      plot(
        df, main = paste("PACF untuk ",input$var_column2, "setelah differencing"),
        ylab = "value", col = 'red'
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
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Hasil penaksiran parameter dengan menggunakan metode least square yang sudah ditabelkan')
  )
  )
  
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
      kolom <- input$var_column3
      if (length(kolom) == 0){
        kolom <- input$var_column
      }
      va <- var_analysis()
      pr <- predict(va, n.ahead = input$fcst.time)
      tbl <- pr$fcst[[kolom]]
      rownames(tbl) <- as.character(c(1:input$fcst.time))
      tbl
    }
  }, escape = FALSE,
  options = list(
    paging = FALSE,
    processing = FALSE,
    searching = FALSE
  ),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Hasil forecasting titik maupun interval  maksimum 30 hasil prediksi')
  ))
  
  output$irf_plot <- renderPlot({
    if (length(data_sumber()) > 0) {
      tst  <- input$var_column3
      get_forecasting()
      
    }
  })
  
  output$fcst_anti.trans <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      
      num_of_col = length(names(anu))
      num_of_row = length(anu[[1]]$anti.trans)
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$anti.trans
        tbl[,col] <- datanya
      }
      
      tbl
    }
  }, option = list(searching = FALSE,
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Hasil anti transformasi data forecasting')
  ))
  
  output$fcst_anti.diff <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      dt <- data_sumber()
      anu <- ambil_data()
      
      num_of_col = length(names(anu))
      num_of_row = length(dt[,1])
      tot_elm = num_of_row * num_of_col
      tbl <- array(1:tot_elm, dim = c(num_of_row, num_of_col))
      colnames(tbl) <- names(anu)
      
      for (col in 1:num_of_col) {
        nama_kolom <- colnames(tbl)[col]
        datanya <- anu[[col]]$anti.diff
        ln <- length(datanya)
        if (ln < num_of_row) { # karena level diff setiap kolom bisa berbeda
          selisih <- num_of_row - ln
          for (i in 1:selisih)
            datanya <- c(datanya, NA)
        }
        
        tbl[,col] <- datanya
      }
      
      tbl
    }
  }, option = list(searching = FALSE,
                   rownames = FALSE),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::strong('Hasil anti differencing data forecasting')
  )
  )
  
  output$summary_estimasi_hasil <- renderPrint({
    if (length(data_sumber()) > 0) {
      var_analysis()
    }
  })
  
  output$summary_diag_serial <- renderPrint({
    if (length(data_sumber()) > 0) {
      va <- var_analysis()
      serial.test(va, lags.pt = 16)
    }
  })
  
  output$summary_diag_normal <- renderPrint({
    if (length(data_sumber()) > 0) {
      va <- var_analysis()
      normality.test(va)
    }
  })
  
  output$summary_fcst_tbl <- DT::renderDataTable({
    if (length(data_sumber()) > 0) {
      va <- var_analysis()
      pr <- predict(va, n.ahead = 30)
      tbl <- pr$fcst[[input$var_column3]]
      rownames(tbl) <- as.character(c(1:30))
      tbl
    }
  }, escape = FALSE,
  options = list(
    paging = FALSE,
    processing = FALSE,
    searching = FALSE
  ))
  
})
