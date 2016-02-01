shinyUI(
  navbarPage(img(src = "tulisan.png"),
    windowTitle = "Model Proyeksi Pertumbuhan Ekonomi Jabar", 	
    header = h1(style="text-align:center;","Model Proyeksi Pertumbuhan Ekonomi Jabar"),
    fluid = TRUE,
    tabPanel("Data",
      mainPanel(
        tabsetPanel(
          tabPanel("Data upload",
            fileInput("sumber_data", 
              label = "upload"
            ),
            DT::dataTableOutput("data_table")
          ),
          tabPanel("Data summary", 
            tableOutput("data_summary")
          )
        )
      )
    ),
    tabPanel("Plot",
      mainPanel(
        strong("Plots"),
        uiOutput("pilih_kolom"),
        br(),
        plotOutput("asli_ts"),
        br(),
        plotOutput("asli_acf"),
        br(),
        plotOutput("asli_pacf")
      )
    ),
    tabPanel("Identifikasi",
      mainPanel(
        tabsetPanel(
          tabPanel("Data Hasil Transformasi", 
            DT::dataTableOutput("lambda_transformasi"),
            br(),
            DT::dataTableOutput("data_transformasi")
          ),
          tabPanel("Hasil Test ADF",
            fluidRow(
              column(3,
                selectInput('adf_diff_level', 
                  label='Level Differencing', 
                  choices = c(1,2,3)
                )
              ),
              column(3,
                uiOutput("pilih_kolom2")
              )
            ),
            tableOutput("hasil_adf"),
            span("Hasil pengujian ADF, jika nilai p-value< 0,05 berarti data sudah stasioner"),
            plotOutput('id_ts'),
            plotOutput('id_acf'),
            plotOutput('id_pacf')
          ),
          tabPanel("Data Hasil Differencing",
            selectInput('diff_level', label='Level', choices = c(0,1,2,3)),
            conditionalPanel('input.diff_level == 0', 
              tableOutput("diff_summary"),
              span("Data setelah di lakukan differensing")
            ),
            conditionalPanel('input.diff_level == 1',
              DT::dataTableOutput("data_differencing1")
            ),
            conditionalPanel('input.diff_level == 2',
              DT::dataTableOutput("data_differencing2")
            ),
            conditionalPanel('input.diff_level == 3',
              DT::dataTableOutput("data_differencing3")
            )
          ),
          tabPanel("Penentuan Lag", 
            verbatimTextOutput("var_select"),
            span("Penentuan Lag untuk menentukan model terbaik"),
            span("ditentukan berdasarkan nilai mayoritas(terbanyak) dan nilai lag terkecil")
          )
        )
      )
    ),
    tabPanel("Estimasi",
      mainPanel(
        tabsetPanel(
          tabPanel("Analisis VAR",
            selectInput(
             "estimasi_type",
             label = "Tipe",
             choices = c('Constant' = 'const', 'Trend' = 'trend', 'Both' = 'both', 'None' = 'none'),
             selected = 'const'
            ),
            span("Hasil penaksiran parameter dengan menggunakan metode least square"),
            br(),
            verbatimTextOutput("estimasi_hasil")
          ),
          tabPanel("Summary",
            DT::dataTableOutput('estimasi_kesimpulan')
          )
        )
      )
    ),
    tabPanel("Diagnostik",
      mainPanel(
        tabPanel('Serial Test',
          span("Hasil Diagnostik check jika p-value>5% maka residual sudah independent"),
          br(),
          verbatimTextOutput('diagnostic_serial')
          
        ),
        tabPanel('Normality Test', 
          span("Hasil Uji Normalitas jika p-value>5% maka residual sudah mengikuti distribusi Normal"),
          br(),
          verbatimTextOutput('diagnostic_normal')
          
        )
      )
    ),
    tabPanel("Forecasting",
      mainPanel(
        tabsetPanel(
          tabPanel('Forecasting',
            fluidRow(
              column(3,
                sliderInput("fcst.time",
                  h4("Prediksi"),
                  min = 1,
                  max = 30,
                  value = 5
                )
              ),
              column(3,
                    uiOutput("pilih_kolom3")
              )
            ),
            DT::dataTableOutput('fcst_tbl'),
            br(),
            plotOutput('irf_plot'),
            span("lamanya pengaruh dari shock suatu variabel terhadap variabel lain sampai pengaruhnya hilang atau kembali ke titik keseimbangan")
          ),
          tabPanel('Anti Transformasi', 
            DT::dataTableOutput('fcst_anti.trans')
          ),
          tabPanel('Anti Differencing', 
            DT::dataTableOutput('fcst_anti.diff')
          )
        )
      )         
    ),
    tabPanel("Summary Analysis",
      mainPanel(
        span("Model terbaik berdasarkan penentuan lag pada sub menu penentuan lag"),
        br(),
        verbatimTextOutput("summary_estimasi_hasil"),
        verbatimTextOutput('summary_diag_serial'),
        verbatimTextOutput('summary_diag_normal'),
        DT::dataTableOutput('summary_fcst_tbl')
      )
    ),
    tabPanel("Help",
      mainPanel(
        br(),
        p(span("Sebelum memakai aplikasi, upload file yang akan dianalisis menggunakan tombol [Browse] yang ada disebelah kiri"),  style="color:#990000"),
        h4(strong("Penjelasan Menu")),
        p(strong("Data"), "Untuk melihat isi data yang sudah di-upload."),
        p(strong("Plots"), "Untuk melihat plot dari data yang di-upload."),
        p(strong("Identifikasi"),"Di menu ini data diolah menjadi stasioner."),
        p(strong("Estimasi"), "Menerapkan teknik VAR(Vector Auto Regressive) pada data yang sudah diolah di menu 'Identifikasi'."),
        p(strong("Diagnostic"), "Melakukan pengujian pada hasil perhitungan VAR."),
        p(strong("Forecasting"), "Melakukan prediksi sampai 30 tahun kedepan"),
        p(strong("Summary Analisis"), "Menampilkan kesimpulan dari proses estimasi sampai prediksi"),
        br()
      )
    )
  )
)

