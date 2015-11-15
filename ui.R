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
  
  titlePanel("Prediksi Ekonomi Jawa Barat"),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      #textInput("ticker", h3("Security ticker"), value="SPY"),
      
      h3("Pengaturan VAR"),
      
      fileInput("sumber_data", label = "pilih data"),
      
      sliderInput(
        "lag_max", h4("Interval Lag"),
        min = 1,
        max = 10,
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
        tabPanel(
          strong("Plots"),
          br(),
          plotOutput("var_fit"),
          br(),
          plotOutput("acf_residual"),
          br(),
          plotOutput("pacf_residual")
        ),
        
        tabPanel(strong("Summary"),
                 br(),
                 tableOutput("var_select"),
                 br(),
                 br()),
        
        tabPanel(strong("Data"),
                 tabsetPanel(
                   tabPanel("Data upload", tableOutput("data_table")),
                   tabPanel("Data summary", tableOutput("data_summary"))
                 )),
        
        tabPanel(
          strong("Test"),
          tabsetPanel(
            tabPanel("Residual", plotOutput("residual")),
            tabPanel("Stability", plotOutput("stability")),
            position = 'right'
            
          )
        )
        
      )
    )
  )
      ))
