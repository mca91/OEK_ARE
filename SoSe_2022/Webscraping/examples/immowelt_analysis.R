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

url_1 <- 'https://www.immowelt.de/expose/25pp95e'
url <- 'https://www.immowelt.de/expose/25tux5c'
html <- read_html(url)
html_1 <- read_html(url_1)

###############
#### tries ####
###############

html %>%
  html_nodes('#exposeAddress div') %>% #  #exposeAddress div
  html_text() %>%
  str_trim() %>%
  .[1] %>%
  readr::parse_number(locale = locale(decimal_mark = ",", 
                                      grouping_mark = "."))
  

html_1 %>%
  html_nodes('#exposeAddress div:nth-child(1)') %>% #  #exposeAddress div
  html_text() %>%
  str_trim() %>%
  .[1] %>%
  readr::parse_number(locale = locale(decimal_mark = ",", 
                                      grouping_mark = "."))

### subort

html_1 %>%
  html_nodes('#exposeAddress div:nth-child(1)') %>% #  #exposeAddress div
  html_text() %>%
  str_trim()

html %>%
  html_nodes('#exposeAddress div') %>% #  #exposeAddress div
  html_text() %>%
  str_trim() %>%
  str_replace_all("\\(|\\)|\\/", "") %>%
  matrix(nrow = 1) %>%
  as_tibble() %>%
  unite(V1, V2)


html_1 %>%
  html_nodes('#exposeAddress div') %>% #  #exposeAddress div
  html_text() %>%
  str_trim() %>%
  str_replace_all("\\(|\\)|\\/|[0-9]+", "") %>%
  str_replace_all("\\-", " ") %>%
  str_replace_all('Bochum|Essen', '') %>%
  str_trim() %>%
  matrix(nrow = 1) %>%
  as_tibble() %>%
  tidyr::unite(a, everything())


%>%
  unite(V1, V2)

  tibble::as_tibble_row(.name_repair = 'unique') %>%
  unite(...1, ...2)



  
  readr::parse_character()




html_1 %>%
  html_nodes('#exposeAddress div:nth-child(1)') %>% #  #exposeAddress div
  html_text() %>%
  str_trim() 

html %>%
  html_nodes('#exposeAddress div:nth-child(1)') %>% #  #exposeAddress div
  html_text() %>%
  str_trim() %>%
  str_replace_all("\\(|\\)|\\/", "")



#%>%
  
  readr::parse_number(locale = locale(decimal_mark = ",", 
                                      grouping_mark = "."))


  
  .[1] %>%
  str_replace_all("\\(|\\)|\\/", "") %>%
  str_split(' ') %>%
  unlist() %>%

  
  
  
  
  readr::parse_number(value, 
                      locale = locale(decimal_mark = ",", 
                                      grouping_mark = ".")
                      
                      
                      
                      
  matrix(nrow = 1) %>%
  as_tibble() %>%
  dplyr::rename('zipcode' = 1, 
                'city' = 2,
                'district' = 3
  )
return(df)