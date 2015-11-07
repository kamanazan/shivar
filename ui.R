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
    
    h3("VAR Type"),
    
    selectInput("var_type", 
                label = "choose type",
                choices = c("both", "const",
                            "all"),
                selected = "both"),
    
    br(),
    
    h3("Lag Interval"),
    
    sliderInput("lag_max", h4("returns distribution"),
                min = 1,
                max = 10,
                value = 1
                ),
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