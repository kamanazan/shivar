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
            tabPanel("Data Hasil Transformasi", 
                     DT::dataTableOutput("lambda_transformasi"),
                     br(),
                     DT::dataTableOutput("data_transformasi")),
            tabPanel("Hasil Test ADF",
                     selectInput('adf_diff_level', label='Level Differencing', choices = c(1,2,3)),
                     tableOutput("hasil_adf"),
                     plotOutput('id_ts'),
                     plotOutput('id_acf'),
                     plotOutput('id_pacf')),
            tabPanel("Data Hasil Differencing",
                     selectInput('diff_level', label='Level', choices = c(0,1,2,3)),
                     conditionalPanel('input.diff_level == 0', 
                                      tableOutput("diff_summary")),
                     conditionalPanel('input.diff_level == 1',
                                      DT::dataTableOutput("data_differencing1")),
                     conditionalPanel('input.diff_level == 2',
                                      DT::dataTableOutput("data_differencing2")),
                     conditionalPanel('input.diff_level == 3',
                                      DT::dataTableOutput("data_differencing3"))
                     
            ),
            tabPanel("Penentuan Lag", verbatimTextOutput("var_select"))
            #tabPanel("Data Hasil Differencing 3", DT::dataTableOutput("data_differencing3")),
            
          )),
        
        tabPanel(
          strong("Estimasi"),
          tabsetPanel(
            tabPanel("Analisis VAR",
                     selectInput(
                       "estimasi_type",
                       label = "Tipe",
                       choices = c('Constant' = 'const', 'Trend' = 'trend', 'Both' = 'both', 'None' = 'none'),
                       selected = 'const'
                     ),
                    verbatimTextOutput("estimasi_hasil")
            ),
            tabPanel("Summary",
                     DT::dataTableOutput('estimasi_kesimpulan')
            )
          )
          
        ),
        tabPanel(strong('Diagnostic'), tabsetPanel(
          tabPanel('Serial Test', verbatimTextOutput('diagnostic_serial')),
          tabPanel('Normality Test', verbatimTextOutput('diagnostic_normal'))
        )),
        tabPanel(
          strong('Forecasting'),
          tabsetPanel(
            tabPanel('Forecasting',
                   sliderInput(
                     "fcst.time", h4("Prediksi"),
                     min = 1,
                     max = 30,
                     value = 5
                   ),
                   DT::dataTableOutput('fcst_tbl'),
                   br(),
                   plotOutput('irf_plot')
          ),
          tabPanel('Anti Transformasi', DT::dataTableOutput('fcst_anti.trans')),
          tabPanel('Anti Differencing', DT::dataTableOutput('fcst_anti.diff'))
        )
       )
                 
        
      )
    )
  )
      ))
