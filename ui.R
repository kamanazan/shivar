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
      
      uiOutput('pilih_kolom'),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        position = c("above"),
        tabPanel(strong("Data"),
                 tabsetPanel(
                   tabPanel("Data upload", DT::dataTableOutput("data_table")),
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
            tabPanel("Data Hasil Transformasi", DT::dataTableOutput("data_transformasi")),
            tabPanel("Data Hasil Differencing 1", DT::dataTableOutput("data_differencing1")),
            tabPanel("Data Hasil Differencing 2", DT::dataTableOutput("data_differencing2")),
            tabPanel("Data Hasil Differencing 3", DT::dataTableOutput("data_differencing3")),
            tabPanel("Hasil Test ADF", 
                     tableOutput("hasil_adf"),
                     plotOutput('id_ts'),
                     plotOutput('id_acf'),
                     plotOutput('id_pacf'))
          )),
        
        tabPanel(
          strong("Estimasi"),
          tabsetPanel(
            tabPanel("Analisis VAR",
                     fluidRow(
                       column(4,
                              selectInput(
                                "estimasi_type",
                                label = "Tipe",
                                choices = c('Constant' = 'const', 'Trend' = 'trend', 'Both' = 'both', 'None' = 'none'),
                                selected = 'const'
                       )),
                       column(4,
                              sliderInput(
                                "estimasi_p.val", h4("Nilai P"),
                                min = 1,
                                max = 5,
                                value = 1
                      ))
                    ),
                    verbatimTextOutput("estimasi_hasil")
            ),
            tabPanel("Summary",
                     DT::dataTableOutput('estimasi_kesimpulan')
            )
          )
          
        )
        
      )
    )
  )
      ))
