source('helper.R')

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(waiter)
library(shinyjs)

# Set default port for development
# options(shiny.port = 3535)

# Setup loading screen
loading_screen <- tagList(
    spin_facebook(),
    h4("Loading...")
)

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme('darkly'),
    title = 'Stock Analyzer',
    selected = 'Descriptive Analysis',
    useShinyjs(),
    tabPanel(
        title = 'Descriptive Analysis', 
        use_waiter(),
        waiter_show_on_load(html = loading_screen),
        sidebarLayout(
            mainPanel(
                verticalLayout(
                    highchartOutput(outputId = 'plot', height = '500px') %>% withSpinner(),
                    br(),
                    wellPanel(
                        h3('Analyst Commentary'),
                        textOutput('commentary')
                    )
                )
            ),
            sidebarPanel(
                verticalLayout(
                    h2('Stock Analyzer'),
                    helpText(tagList("Data source:", a("Yahoo Finance", href = "https://finance.yahoo.com/", target="_blank"))),
                    selectInput(
                        inputId = "tickerSelector", 
                        label = "Stock List (Pick One to Analyze)",
                        choices = paste(df_ticker$ticker, df_ticker$name, sep = ', '),
                        selected = 'BBCA.JK, PT Bank Central Asia Tbk'
                    ),
                    shinyjs::hidden(
                        div(
                            id = 'optionsDiv',
                            checkboxGroupInput(
                            inputId = 'optionsCheckbox', label = 'Available Options:',
                            c('Show Moving Average (MA)' = 'showMA',
                              'Show Bolinger Bands' = 'showBB',
                              'Show Volume' = 'showVolume',
                              'Show Relative Strength Index' = 'showRSI',
                              'Show Moving Average Convergence Divergence (MACD)' = 'showMACD'),
                            selected = c('showVolume','showMA', 'showRSI')
                            ),
                            div(
                                id = 'optionsForMA',
                                HTML('<h4><strong>Moving Average Options</strong></h4>'),
                                radioButtons(
                                    inputId = 'SMAorEMA',
                                    label = 'Moving Average Type:',
                                    c('Simple' = 'sma',
                                      'Exponential' = 'ema'),
                                    selected = 'sma'
                                ),
                                sliderInput(
                                    inputId = 'short_ma',
                                    label = 'Short Moving Average',
                                    min = 3,
                                    max = 90,
                                    value = 20,
                                    step = 1
                                ),
                                sliderInput(
                                    inputId = 'long_ma',
                                    label = 'Long Moving Average',
                                    min = 100,
                                    max = 366,
                                    value = 100,
                                    step = 1
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            width = 5,
                            actionButton("analyzeButton", "Analyze", class = 'btn btn-primary rounded', width = '100%')
                        ),
                        column(
                            width = 1, 
                            offset = 5,
                            actionButton('optionsButton', '', icon = icon('cog'), class = 'btn btn-primary rounded')
                        )
                    )
                )
            )
        )
    ),
    tabPanel(
        title = 'Predictions',
        h1('COMING SOON')
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    w <- Waiter$new(id = 'plot')
    
    show_volume <- reactive({any(input$optionsCheckbox == 'showVolume')})
    show_ma <- reactive({any(input$optionsCheckbox == 'showMA')})
    show_bbands <- reactive({any(input$optionsCheckbox == 'showBB')})
    show_rsi <- reactive({any(input$optionsCheckbox == 'showRSI')})
    show_macd <- reactive({any(input$optionsCheckbox == 'showMACD')})
    ticker_split <- reactive({stringr::str_split_fixed(toString(input$tickerSelector), ', ', n = 2)})
    
    observeEvent(input$short_ma, {
        updateSliderInput(session, "long_ma", min = input$short_ma + 10)
    })
    
    observeEvent(input$optionsButton, {
        shinyjs::toggle('optionsDiv', time = 0.5, anim = TRUE, animType = "slide")
    })
    
    observeEvent(input$optionsCheckbox, {
        if (show_ma()){
            shinyjs::show('optionsForMA', time = 0.5, anim = TRUE, animType = "slide")
        } else {
            shinyjs::hide('optionsForMA', time = 0.5, anim = TRUE, animType = "slide")
        }
    }, ignoreNULL = FALSE)
    
    df <- reactive({loadTickerDataFrame(ticker_split()[1])})
    short_ma <- reactive({
        if (input$SMAorEMA == 'sma'){
            SMA(df()[, 4], input$short_ma)
        } else {
            EMA(df()[, 4], input$short_ma)
        }
    })
    long_ma <- reactive({
        if (input$SMAorEMA == 'sma'){
            SMA(df()[, 4], input$long_ma)
        } else {
            EMA(df()[, 4], input$long_ma)
        }
    })
    
    commentary_message <- eventReactive(input$analyzeButton, {
        last_index <- length(short_ma())
        short <- short_ma()[last_index]
        long <- long_ma()[last_index]
        line_position <- ifelse(short > long, 'above', 'below')
        line_trend <- ifelse(short > long, 'positive', 'negative')
        selected_ma <- ifelse(input$SMAorEMA == 'sma', 'simple', 'exponential')
        paste(
            'Quick review on',
            ticker_split()[2],
            paste('(', ticker_split()[1], ')', sep = ''),
            'stock based on',
            selected_ma,
            'moving average: the short moving average',
            paste('(', input$short_ma, '-day) is', sep = ''),
            line_position,
            'the long moving average',
            paste('(', input$long_ma, '-day), indicating a', sep = ''),
            line_trend,
            'trend.',
            sep = ' '
        )
    }, ignoreNULL = FALSE)
    
    draw_plot <- eventReactive(input$analyzeButton, {
        hcPlotStock(
            df = df(), 
            ticker_name = ticker_split()[2], 
            ticker = ticker_split()[1], 
            show_ma = show_ma(), 
            show_bbands = show_bbands(), 
            show_volume = show_volume(), 
            show_macd = show_macd(), 
            show_rsi = show_rsi(), 
            short_ma = short_ma(), 
            long_ma = long_ma()
        )
    }, ignoreNULL = FALSE)
    
    waiter_hide()
    
    output$plot <- renderHighchart({draw_plot()})
    output$commentary <- renderText({commentary_message()})
    
}

# Run the application 
shinyApp(ui = ui, server = server)