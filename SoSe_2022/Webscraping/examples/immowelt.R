library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)
library(magrittr)


locations <- c('essen', 'bochum')


url <- 'https://www.immowelt.de/liste/essen/wohnungen/mieten?sort=relevanz'
url <- 'https://www.immowelt.de/liste/essen/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=1'

#---- getting links ----

# getting links 

extract_links <- function(url){
  
  tibble(url = read_html(url) %>%
           html_elements('.noProject-eaed4') %>%
           html_attr('href')
  )
}

get_links<- function(locations, num_pages){
  i <- 1
  repeat{
    if(i == 1){
      df <- extract_links(paste0('https://www.immowelt.de/liste/' , locations,
                                 '/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=', i))
      print(paste(locations, i))
    } else {
      temp_df <- extract_links(paste0('https://www.immowelt.de/liste/' , locations,
                                      '/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=', i))
      if(any(duplicated(rbind(df, temp_df)))) break
      df <- rbind(df, temp_df)
      print(paste(locations, i))
      if(i == num_pages) break
    }
    i <- i + 1
    Sys.sleep(sample(2:10, 1))
  }
  return(df)
}



# getting dataframe 
data <- map2_dfr(locations, c(2, 2), get_links) #, .id = "company") 

html <- read_html(data[[1,1]])

extract_page_data <- function(url, ...){
  html <- read_html(url) 
 df <-  tibble(title = get_title(html),
         get_location(html),
         warm_rent = get_warm.rent(html),
         cold_rent = get_cold.rent(html),
         deposite = get_deposit(html), 
         get_size(html) ,
         building_type = get_building.type(html),
         energy_class = get_energy.class(html),
         energy_consumption = get_energy.cons(html),
         build_year = get_year(html)
  )
 
 return(df)
 
 Sys.sleep(sample(2:15, 1))
}

data_try <- data %>%
  slice_sample(n = 3)

data_try %<>%
  purrr::pmap_dfr(extract_page_data)
  
url <- data_try[[3,1]]

extract_page_data(data_try[[1,1]])

########## functions 
# title 
get_title <- function(html){
  html |>
    html_nodes('h1') |> 
    html_text() |> 
    str_trim()
}

### locations (zipcode)
get_location <- function(html){
 df <- html |>
   html_nodes('#exposeAddress div') |> 
   html_text() |> 
   str_trim() |>
   str_split(' ') |> 
   unlist() |>
   str_replace_all("\\(|\\)", "") |>
   matrix(nrow = 1) |>
   as_tibble() |>
   dplyr::rename('zipcode' = 1, 
                 'city' = 2,
                 'district' = 3
   )
 return(df)
}

get_location(html)

# prices


html <- read_html(data_try[[2,1]])
# Warmmiete
get_warm.rent <- function(html){
   
df <- html %>%
  html_nodes('#aPreise .cell__row') %>% 
  html_text() %>%
  str_trim() %>%
  as_tibble() %>%
  dplyr::filter(stringr::str_detect(value, 'Warmmiete') & !stringr::str_detect(value, 'Heizkosten')) %>%
  pull() 

  ifelse(length(df) == 0, NA, 
             readr::parse_number(df, locale = locale(decimal_mark = ",", grouping_mark = ".")))

}

get_warm.rent(html)

get_cold.rent <- function(html){
  
  df <- html %>%
    html_nodes('#aPreise .cell__row') %>%  #.cell__row:nth-child(4)
    html_text() %>%
    str_trim() %>%
    as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, 'Kaltmiete')) %>%
    pull() 
  
  ifelse(length(df) == 0, NA, 
           readr::parse_number(df, locale = locale(decimal_mark = ",", grouping_mark = ".")))
}


get_prices(html)

# rent, square meter, rooms
get_size <- function(html){
  html %>%
    html_nodes('.has-font-300') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",")) %>%
    matrix(nrow = 1) %>%
    as_tibble() %>%
    dplyr::select(-1) %>%
    dplyr::rename('square_meter' = 1, 
                  'rooms' = 2
    )
}

get_size(html)

# energy class
get_energy.class <- function(html){
  html %>%
    html_nodes('.ng-star-inserted:nth-child(5) p') %>% 
    html_text() %>% 
    str_trim() %>%
    .[2]
}

get_energy.class(html)

# build year
get_year <- function(html){
html %>%
  html_nodes('app-energy .cell:nth-child(3) p') %>% 
  html_text() %>% 
  str_trim() %>%
  .[2]
}

get_year(html)


# deposit
get_deposit <- function(html){
  html %>%
    html_nodes('#aPreise .ng-star-inserted .card-content') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
}

get_deposit(html)

# energy consumption
get_energy.cons <- function(html){
  html %>%
    html_nodes('.ng-star-inserted:nth-child(6) .has-font-75+ p') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
}

get_energy.cons(html)

# Building type
get_building.type <- function(html){
  html %>%
    html_nodes('.energy_information .ng-star-inserted:nth-child(2) .has-font-75+ p') %>% 
    html_text() %>% 
    str_trim()
}

get_building.type(html)


#####################
######## try ########
#####################
# all prices 
html %>%
  html_nodes('#aPreise .card') %>% 
  html_text() %>% 
  str_trim() 
  

html_sub %>%
  html_nodes('#aPreise .card') %>% 
  html_text() %>% 
  str_trim()
  
  


html %>%
  html_nodes('.hardfact__label') %>% 
  html_text() %>% 
  str_trim() %>%
  readr::parse_number()

# rent, square meter, rooms
html %>%
  html_nodes('.has-font-300') %>% 
  html_text() %>% 
  str_trim() %>%
  stringr::str_replace_all(',', '.') %>%
  readr::parse_number()

# Energy 

# energy class
html %>%
  html_nodes('.ng-star-inserted:nth-child(5) p') %>% 
  html_text() %>% 
  str_trim() 


# energy consumption
html %>%
  html_nodes('.ng-star-inserted:nth-child(6) .has-font-75+ p') %>% 
  html_text() %>% 
  str_trim() 


# Building type
html %>%
  html_nodes('.energy_information .ng-star-inserted:nth-child(2) .has-font-75+ p') %>% 
  html_text() %>% 
  str_trim() 





# ausstattung
html %>%
  html_nodes('app-details .ng-star-inserted') %>% 
  html_text() %>% 
  str_trim() 
















