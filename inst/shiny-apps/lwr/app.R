library(shiny)

month_f <- function(month) {
  switch(month,
    January = 1,
    February = 2,
    March = 3,
    April = 4,
    May = 5,
    June = 6,
    July = 7,
    August = 8,
    September = 9,
    October = 10,
    November = 11,
    December = 12
  )
}

quarter_f <- function(quarter) {
  switch(quarter,
    Q1 = 1,
    Q2 = 2,
    Q3 = 3,
    Q4 = 4
  )
}

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Decomposition of Seasonal Time Series"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
              tabPanel("I. Data import",
                 fileInput("upload", "Upload a data file", buttonLabel = "Upload ...", multiple = FALSE),
                 uiOutput("filesettings"),
                 uiOutput("varselection"),
                 uiOutput("freq"),
                 uiOutput("startdate"),
                 uiOutput("logtransf")
              ),
              tabPanel("II. Decomposition",
                 uiOutput("decompmethod"),
                 uiOutput("decompsettings")
              ),
              tabPanel("III. Data export",
                 downloadButton("dlData", label = "Download decomposed data") 
              )
            )#,
            
            # ,
            # downloadButton("dlData", label = "Download decomposed data")            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Data frames",
              uiOutput("nentries"),
              tabsetPanel(
                tabPanel("Input data",
                  uiOutput("dfinput")  
                ),
                tabPanel("Decomposed data",
                  uiOutput("dfoutput")  
                )
              )
            ),
            tabPanel("Plots",
              sliderInput("figheight", "Plot height:", min = 100, max = 1800, step = 20, value = 500),
              sliderInput("figwidth", "Plot width:", min = 100, max = 1800, step = 20, value = 1200),                     
              tabsetPanel(
                tabPanel("Plot of observations",
                  uiOutput("obsplot")
                ),              
                tabPanel("Fitted values",
                  uiOutput("fitplot")      
                ),
                tabPanel("Residuals",
                  uiOutput("resplot")
                )
              )
              
            )            
          )
        )
    )
)

# Define server logic
server <- function(input, output) {
  
    output$filesettings <- renderUI({
      req(input$upload)
      list(
        checkboxInput("header", label = "Header?", value = TRUE),
        selectInput("sep", "Separator:", choices = c(",", ";", "."), selected = ",")
      )
    })
    
    dataset <- reactive({
      req(input$sep)
      req(input$header)
      
      ext <- tools::file_ext(input$upload$name)
      switch(
        ext,
        csv = read.csv(input$upload$datapath, header = input$header, sep = input$sep),
        txt = read.table(input$upload$datapath, header = input$header, sep = input$sep),
        validate("Infalid file type provided. Please only upload .csv or .txt files.")
      )
    })    
    
    output$nentries <- renderUI({
      req(dataset())
      sliderInput("nvals", "Entries to show:", min = 1, max = length(dataset()[, 1]), step = 1, value = 5)
    })
    
    output$dfinput <- renderUI({
      req(dataset())
      tableOutput("datatable")   
    })
    
    output$datatable <- renderTable({
      req(dataset())
      req(input$nvals)
      dataset()[1:input$nvals, , drop = FALSE]
    })
    
    choices <- reactive({
      req(dataset())
      c("", names(dataset()))
    })
    
    output$varselection <- renderUI({
      req(choices())
      
      list(
        selectInput("tsvar", "Select time series variable:", 
                    choices = choices(), selected = choices()[[1]])       
      )
    })
    
    output$freq <- renderUI({
      req(input$tsvar)
      
      sliderInput("frequ", "Seasonal period:", min = 4, max = 12, step = 8,
                  value = 4)
    })
    
    output$startdate <- renderUI({
      req(input$frequ)
      
      a <- sliderInput("year", "Year of the first observation:", min = 1800, max = 2020,
                  value = 1950, step = 1, sep = "")
      if (input$frequ == 4) {
        b <- selectInput("subd", "Quarter of the first observation:",
                         choices = paste0("Q", 1:4), selected = "Q1")
      } else if (input$frequ == 12) {
        b <- selectInput("subd", "Month of the first observation:",
                         choices = month.name, selected = "January")        
      }
      list(a, b)
    })
    
    tseries <- reactive({
      req(input$subd)
      
      subd <- input$subd
      if (input$frequ == 4) {
        subd <- quarter_f(subd)
      } else if (input$frequ == 12) {
        subd <- month_f(subd)
      }
      y <- as.character(input$tsvar)
      
      yt <- ts(dataset()[[y]], start = c(input$year, subd), frequency = input$frequ)
      yt
    })
    
    output$logtransf <- renderUI({
      req(tseries())
      list(
        checkboxInput("logs", label = "Log-transformation?", value = FALSE)
      )
    })
    
    ts_tr <- reactive({
      req(tseries())
      
      if (isTRUE(input$logs)) {
        out <- log(tseries())
      } else if (isFALSE(input$logs)) {
        out <- tseries()
      }
      out
    })
    
    ts_name_tr <- reactive({
      req(ts_tr())
      
      if (isTRUE(input$logs)) {
        out <- paste0("log(", input$tsvar, ")")
      } else if (isFALSE(input$logs)) {
        out <- input$tsvar
      }
      out      
    })
    
    output$obsplot <- renderUI({
      req(ts_tr())
      req(input$figheight)
      req(input$figwidth)
      
      plotOutput("plotobs", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })
    
    output$plotobs <- renderPlot({
      req(ts_tr())
      req(input$figheight)
      req(input$figwidth)      
      
      pout <- zoo::autoplot.zoo(ts_tr()) +
        ggplot2::theme_bw() +
        ggplot2::ylab(ts_name_tr()) +
        ggplot2::xlab("Time") +
        ggplot2::ggtitle(paste0('Plot of the time series "', ts_name_tr(), '"')) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
      suppressWarnings(print(pout))
    })
    
    output$decompmethod <- renderUI({
      req(tseries())
      selectInput("method", "Decomposition method:",
                  choices = c("Locally weighted regression", "Berlin Procedure 4.1",
                              "Linear regression", "Local linear regression", "Moving average", "Hamilton filter"),
                  selected = "Locally weighted regression")
    })
    
    output$decompsettings <- renderUI({
      req(input$method)
      
      if (input$method == "Locally weighted regression") {
        out <- list(
            sliderInput("opoly",
                        "Order of trend polynomial:",
                        min = 1,
                        max = 3,
                        value = 1,
                        step = 2),
            selectInput("kfun",
                        "Kernel function:",
                        choices = c(
                          "uniform",
                          "epanechnikov",
                          "bisquare",
                          "triweight"
                        ), selected = "epanechnikov", multiple = FALSE),  
            sliderInput("bwidth",
                        "Bandwidth:",
                        min = 0.01,
                        max = 0.4,
                        value = 0.1,
                        step = 0.005),
            selectInput("infR", "Inflation rate:", choices = c("optimal", "naive"), selected = "optimal"),
            actionButton("automate", "Automatically select optimal bandwidth")        
        )        
      } else if (input$method == "Berlin Procedure 4.1") {
        out <- list()
      } else if (input$method == "Linear regression") {
        out <- list(
            sliderInput("opoly",
                        "Order of trend polynomial:",
                        min = 1,
                        max = 4,
                        value = 1,
                        step = 1),
            sliderInput("opoly_s",
                        "Order of seasonality polynomial:",
                        min = 0,
                        max = 4,
                        step = 1,
                        value = 1
                        )
        )
        
      } else if (input$method == "Local linear regression") {
        out <- list(
            sliderInput("bwidth_trend",
                        "Bandwidth for trend estimation:",
                        min = 1,
                        max = 50,
                        value = 4,
                        step = 1),
            sliderInput("bwidth_season",
                        "Bandwidth for seasonality estimation:",
                        min = 1,
                        max = 50,
                        step = 1,
                        value = 5
                        ),
            sliderInput("kernel_par",
                        "Kernel function smoothness parameter:",
                        min = 0,
                        max = 5,
                        step = 1,
                        value = 1
                        )
        )        
        
      } else if (input$method == "Moving average") {
        out <- list(
            sliderInput("bwidth_t",
                        "Total bandwidth for trend smoothing:",
                        min = 1,
                        max = 30,
                        value = 4,
                        step = 1),
            sliderInput("bwidth_s",
                        "Total bandwidth for trend + seasonality smoothing:",
                        min = 1,
                        max = 29,
                        step = 2,
                        value = 1
                        )
        )
      } else if (input$method == "Hamilton filter") {
        out <- list(
            sliderInput("bgap",
                        "Backwards time gap:",
                        min = 1,
                        max = 50,
                        value = 2 * input$frequ,
                        step = 1),
            sliderInput("nregr",
                        "Number of regressors:",
                        min = 1,
                        max = 50,
                        value = input$frequ,
                        step = 1)            
        )        
      }
      out

    })    
  
    opts <- reactive({
      req(input$opoly)
      req(input$kfun)
      req(input$bwidth)
      deseats::set_options(order_poly = input$opoly,
                           kernel_fun = input$kfun,
                           bwidth = input$bwidth)
    })
    
    observeEvent(input$automate, {
      req(ts_tr())
      req(opts())
      op <- opts()
      op@bwidth <- NA_real_
      est <- deseats::deseats(ts_tr(), op, inflation_rate = input$infR)
      bopt <- est@bwidth
      updateSliderInput(inputId = "bwidth", value = bopt)
    })
  
    est_result <- reactive({
      
        req(input$method)
      
        if (input$method == "Locally weighted regression") {
          req(input$bwidth)
          req(opts)
          est <- deseats::deseats(ts_tr(), opts())           
        } else if (input$method == "Berlin Procedure 4.1") {
          est <- deseats::BV4.1(ts_tr())
        } else if (input$method == "Linear regression") {
          est <- deseats::lm_decomp(ts_tr(), input$opoly, input$opoly_s, input$frequ)
        } else if (input$method == "Local linear regression") {
          est <- deseats::llin_decomp(ts_tr(), kernel_par = input$kernel_par, bwidth_trend = input$bwidth_trend, bwidth_season = input$bwidth_season, season = input$frequ)          
        } else if (input$method == "Moving average") {
          est <- deseats::ma_decomp(ts_tr(), input$bwidth_t, input$bwidth_s, input$frequ)          
        } else if (input$method == "Hamilton filter") {
          est <- deseats::hamilton_filter(ts_tr(), input$bgap, input$nregr)
        }
 
        est
    })  
  
    output$fitplot <- renderUI({
      req(est_result())
      plotOutput("plotfitted", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })

    output$plotfitted <- renderPlot({

        req(est_result())
        
        deseats::autoplot(est_result(), which = 4) +
          ggplot2::ggtitle(paste0('Observations "', ts_name_tr(), '" together with estimated trend and seasonality')) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
          ggplot2::ylab("Observations and trend + seasonality")


    })    
    
    output$resplot <- renderUI({
      req(est_result())
      plotOutput("plotresid", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })        
    
    output$plotresid <- renderPlot({

        req(est_result())
        
        deseats::autoplot(est_result(), which = "residuals") +
          ggplot2::ggtitle(paste0('Estimated remainder in "', ts_name_tr(), '"')) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
          ggplot2::ylab("Detrended and deseasonalized observation")


    })
    
    output$dfoutput <- renderUI({
      req(est_result())
      req(input$nvals)
      
      tableOutput("outp")
    })
    
    dfout <- reactive({
      req(est_result)
      
      .df <- as.data.frame(est_result()@decomp)
      if (isTRUE(input$logtransf)) {
        .df <- exp(.df)
      } else if (isFALSE(input$logtransf)) {
        .df <- .df
      }
      .df$`Time point` <- stats::time(est_result()@decomp)
      if (input$method != "Hamilton filter") {
        .df <- .df[c(5, 1:4)]      
      } else {
        .df <- .df[c(4, 1:3)]           
      }
      .df
    })
    
    output$outp <- renderTable({
      req(dfout())
      req(input$nvals)

      dfout()[1:input$nvals, , drop = FALSE]
    })
    
    output$dlData <- downloadHandler(
      filename = function() {
        paste0("decomposed data - ",  input$tsvar, " - ", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(dfout())
        write.table(dfout(), file, quote = FALSE, sep = ",", row.names = FALSE,
                    col.names = TRUE)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
