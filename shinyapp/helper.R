options("repos" = c("CRAN" = "https://cran.rstudio.com", 
                    "http://www.omegahat.net/R" = "http://www.omegahat.net/R"))

packs <- c('dplyr', 'quantmod', 'devtools', 'forecast', 'shiny', 'shinythemes', 'stringr', 'shinycssloaders', 'waiter', 'shinyjs')
packs_to_install <- packs[!(packs %in% installed.packages())]
if(length(packs_to_install) > 0){
  install.packages(packs_to_install)
}
if(!'highcharter' %in% installed.packages()){
  install.packages("XML", repos = "http://www.omegahat.net/R")
  install.packages('rlist')
  devtools::install_github("jbkunst/highcharter")
}
if(!'modeltime' %in% installed.packages()){
  devtools::install_github("business-science/modeltime")
}


library(highcharter)
library(forecast)
library(dplyr)
library(quantmod)
library(modeltime)


# df_ticker <- read.csv('data_input/tickers.csv', sep = ';')
df_ticker <- read.csv('https://raw.githubusercontent.com/gstdl/Stock-Analysis-Using-R/master/shinyapp/data_input/tickers.csv', sep = ';')


impute_ts <- function(x){
  x <- ts(x, 1, length(x))
  na.interp(x)
}


impute_df <- function(df){
  for (i in 1:length(df[1,])){
    df[, i] <- df[, i] %>% impute_ts()
  }
  df
}


loadTickerDataFrame <- function(x){
  df <- getSymbols(x, auto.assign= FALSE) %>% impute_df()
  # name <- df_ticker %>%  filter(ticker == x) %>% pull(name) %>% toString()
  # list(df, name, x)
  df
}
  

hcPlotStock <- function(df, ticker_name, ticker, show_ma, show_bbands, show_volume, show_macd, show_rsi, short_ma, long_ma){
  # colnames(df) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
  n_axis <- show_volume + show_macd + show_rsi + 1
  height_axis <- c(2, seq(1, 1, length.out = n_axis - 1))
  y_macd <- 1 + show_volume
  y_rsi <- 1 + show_volume + show_macd
  hcChart <- highchart(type = 'stock') %>% 
    # create axis
    hc_yAxis_multiples(create_yaxis(n_axis, height = height_axis, turnopposite = TRUE)) %>% 
    # decimals setting
    hc_tooltip(valueDecimals = 2)
  
  if(show_volume){
    hcChart <- hcChart %>% 
      ## Add volume
      hc_add_series(df[, 5], color = "orange", yAxis = 1, name = "Volume", type = "column", showInLegend = FALSE)
  }
  
  if(show_macd){
    # calculate MACD
    macd <- MACD(df[, 4], 12, 26, 9) 
    macd$diff <- macd$macd - macd$signal
    diff.high <- ifelse(macd$diff > 0, macd$diff, 0)
    diff.low <- ifelse(macd$diff < 0, macd$diff, 0)
    hcChart <- hcChart %>% 
      # set Bar Chart Stacking Option
      hc_plotOptions(column = list(stacking = "normal")) %>% 
      ## Add MACD
      hc_add_series(diff.high, yAxis = y_macd, name = 'MACD diff', type = 'column', color = 'green', showInLegend = FALSE, enableMouseTracking = FALSE, stack = 'diff') %>% 
      hc_add_series(diff.low, yAxis = y_macd, name = 'MACD diff', type = 'column', color = 'red', showInLegend = FALSE, enableMouseTracking = FALSE, stack = 'diff') %>% 
      hc_add_series(macd$macd, yAxis = y_macd, name = 'MACD', color = 'blue', showInLegend = FALSE) %>% 
      hc_add_series(macd$signal, yAxis = y_macd, name = 'Signal Line', color = 'orange', showInLegend = FALSE)
  }
  
  if(show_rsi){
    hcChart <- hcChart %>%
      ## RSI
      hc_add_series(RSI(df[, 4]), yAxis = y_rsi, name = "Relative Stregth Index", color = hex_to_rgba("green", 0.7), showInLegend = FALSE) %>%
      hc_add_series(xts(rep(70, NROW(df)), index(df)), color = hex_to_rgba("red", 0.7), yAxis = y_rsi, name = "Sell level", enableMouseTracking = FALSE, dashStyle = 'LongDash') %>%
      hc_add_series(xts(rep(30, NROW(df)), index(df)), color = hex_to_rgba("blue", 0.7), yAxis = y_rsi, name = "Buy level", enableMouseTracking = FALSE, dashStyle = 'LongDash') %>% 
      # decimals setting
      hc_tooltip(valueDecimals = 2)
  }  
  
  
  if(show_ma){
    hcChart <- hcChart %>%
      ## Moving Averages
      hc_add_series(short_ma, yAxis = 0, name = 'Short Moving Average', color = 'seagreen', enableMouseTracking = FALSE) %>%
      hc_add_series(long_ma, yAxis = 0, name = 'Long Moving Average', color = 'skyblue', enableMouseTracking = FALSE)
  }
  
  
  if(show_bbands){
    # calculate Bollinger bands position
    bbands <- BBands(df[,2:4])
    hcChart <- hcChart %>%
      ## Add Bollinger Bands
      hc_add_series(bbands[, 'up'], yAxis = 0, name = 'Bollinger Bands', color = '#ccc', enableMouseTracking = FALSE, showInLegend = FALSE) %>%
      hc_add_series(bbands[, 'dn'], yAxis = 0, name = 'Bollinger Bands', color = '#ccc', enableMouseTracking = FALSE) %>% 
      hc_add_series(bbands[, 'mavg'], yAxis = 0, name = 'Moving Average (20)', color = '#E377C2', enableMouseTracking = FALSE)
  }
  
  
  hcChart %>%
    # add title
    hc_title(text = paste(ticker, ticker_name, sep = ' - '), align = 'left', x = 30) %>% 
    # add series
    ## candlechart
    hc_add_series(df, yAxis = 0, name = ticker, showInLegend = FALSE) %>%
    # add legend
    hc_legend(enabled = TRUE, align = 'center') %>% 
    # set theme
    hc_add_theme(hc_theme_db())
}