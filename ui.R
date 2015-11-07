shinyUI(fluidPage(
  
  #withMathJax(), # to display LaTeX-like formulas
  
  tags$head( # CSS insert
    tags$style(HTML("      
                    td, th, tr {
                    padding-right: 10px;
                    padding-left: 10px;
                    border: 1px solid #d3d3d3;
                    }
                    border-color {
                    #d3d3d3
                    }
                    "))
    ),
  
  titlePanel("VAR analysis"),
  
  br(),
  
  sidebarLayout(sidebarPanel(
    
    #textInput("ticker", h3("Security ticker"), value="SPY"),
    
    h3("Pengaturan VAR"),
    
    fileInput("sumber_data", label="pilih data"),
    
    selectInput("var_type", 
                label = "Pilih Tipe Deterministik",
                choices = c("both", "const",
                            "trend","none"),
                selected = "both"),
    
    sliderInput("lag_max", h4("Interval Lag"),
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
    
    tabsetPanel(position=c("right"),
                tabPanel(strong("Plots"), 
                         br(),
                         plotOutput("var_result"),
                         #plotOutput("scatterplot"),
                         #htmlOutput("metrics_stats"),
                         #htmlOutput("metrics_finance"),
                         br()),
                
                tabPanel(strong("Data"), 
                         br(),
                         tableOutput("var_select"),
                         p("..."),
                         br(),
                         #tableOutput("table_tail"),
                         #code("Displays no more than 200 rows."),
                         br()),
                
                tabPanel(strong("Annualized data"),
                         br(),
                         tableOutput("data_table"),
                         br())
                
                )
  ))
    ))