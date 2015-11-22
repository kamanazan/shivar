shinyUI(fluidPage(
  #withMathJax(), # to display LaTeX-like formulas
  
  tags$head(# CSS insert
    tags$style(
      HTML(
        "
        td, th, tr {
        padding-right: 10px;
        padding-left: 10px;
        border: 1px solid #d3d3d3;
        }
        border-color {
        #d3d3d3
        }
        "
      )
      )),
  
  titlePanel("Model Proyeksi Pertumbuhan Ekonomi Jabar"),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      #textInput("ticker", h3("Security ticker"), value="SPY"),
      
      h3("Pengaturan VAR"),
      
      fileInput("sumber_data", label = "pilih data"),
      
      sliderInput(
        "lag_max", h4("Interval Lag"),
        min = 1,
        max = 5,
        value = 1
      ),
      
      # Pemilihan variable yang akan diteliti
      # TODO:
      #1. buat choices dinamis tergantung jenis data
      #   yang digunakan
      #2. Opsi 'semua' untuk menampilkan grafik semua kolom
      uiOutput('pilih_kolom'),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        position = c("above"),
        tabPanel(strong("Data"),
                 tabsetPanel(
                   tabPanel("Data upload", tableOutput("data_table")),
                   tabPanel("Data summary", tableOutput("data_summary"))
                 )),
        
        tabPanel(
          strong("Plots"),
          br(),
          plotOutput("asli_ts"),
          br(),
          plotOutput("asli_acf"),
          br(),
          plotOutput("asli_pacf")
        ),
        
        tabPanel(
          strong("Identifikasi"),
          tabsetPanel(
            tabPanel("Data Hasil Transformasi", tableOutput("data_transformasi")),
            tabPanel("Data Hasil Differencing 1", tableOutput("data_differencing1")),
            tabPanel("Data Hasil Differencing 2", tableOutput("data_differencing2")),
            tabPanel("Data Hasil Differencing 3", tableOutput("data_differencing3")),
            tabPanel("Hasil Test ADF", 
                     tableOutput("hasil_adf"),
                     plotOutput('id_ts'),
                     plotOutput('id_acf'),
                     plotOutput('id_pacf'))
          )),
        
        tabPanel(
          strong("Estimasi"),
          tabsetPanel(
            tabPanel("Constant", verbatimTextOutput("estimasi_const")),
            tabPanel("Trend", verbatimTextOutput("estimasi_trend")),
            tabPanel("Both", verbatimTextOutput("estimasi_both")),
            tabPanel("None", verbatimTextOutput("estimasi_none"))
          ))
        
      )
    )
  )
      ))
