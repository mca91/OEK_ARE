library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)
library(magrittr)

## source own functions
source(here::here('SoSe_2022/Webscraping/examples/functions.R'))

locations <- c('essen', 'bochum')



#---- getting links ----

# getting links 

# getting dataframe 
data <- map2_dfr(locations, c(20, 20), get_links)

# scraping data
data %<>%
  dplyr::mutate(row_num = rownames(.)) %>%
  purrr::pmap_dfr(extract_page_data)

data_592 <- data

save(data_592, file = here::here('SoSe_2022/Webscraping/examples/immo_data_big.RData'))
