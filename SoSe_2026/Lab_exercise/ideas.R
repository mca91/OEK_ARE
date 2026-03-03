#----- packages ----
library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)
library(magrittr)
library(tibble)

# ideas and links

# https://community.rstudio.com/t/automated-web-scraping-with-the-targets-package/116924

# https://tradingeconomics.com/commodity/eu-natural-gas

# https://strom-report.de/electricity-prices-europe/

# https://www.epexspot.com/en/market-data?market_area=DE&trading_date=&delivery_date=2022-04-10&underlying_year=&modality=Continuous&sub_modality=&product=60&data_mode=table&period=


url <- 'https://strom-report.de/electricity-prices-europe/'

html <- read_html(url)

table <- html |>
  html_nodes('.tg-wrap') |>
  rvest::html_table() |>
  as.data.frame()

# repair column names, countries 



#--- new example

url <- 'https://tradingeconomics.com/commodity/eu-natural-gas'
html <- read_html(url)

try <- html %>%
  html_text() %>%
  str_trim() %>% 
  str_match_all('json')






