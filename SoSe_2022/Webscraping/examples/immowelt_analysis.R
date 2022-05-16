# packages 
library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)
library(magrittr)


# load data
load(here::here('SoSe_2022/Webscraping/examples/immo_data.RData'))
#
names(data)


# filter problems out

problemetic_data <- data %>%
  dplyr::filter(!is.na(V4) | !is.na(V5))

# problematic with backslashes 

url <- 'https://www.immowelt.de/expose/25pp95e'

html <- read_html(url)

html %>%
  html_nodes('#exposeAddress div') %>%
  html_text() %>%
  str_trim() %>%
  .[1] %>%
  str_replace_all("\\(|\\)|\\/", "") %>%
  str_split(' ') %>%
  unlist() %>%

  matrix(nrow = 1) %>%
  as_tibble() %>%
  dplyr::rename('zipcode' = 1, 
                'city' = 2,
                'district' = 3
  )
return(df)