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
  
  tibble(urls = read_html(url) %>%
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
data <- map2_dfr(locations, c(5, 5), get_links) #, .id = "company") 

html <- read_html(data[[1,1]])

extract_page_data <- function(url){
  html <- read_html(url) 
  tibble(name = get_title(html),
         loacation = get_location(html),
         prices = get_prices(html)
         
         date = get_date(html),
         rating = get_rating(html), 
         title = get_title(html), 
         review = get_review(html))
  Sys.sleep(sample(2:10, 1))
  
}



## functions 
# title 
get_title <- function(html){
  html |>
    html_nodes('h1') |> 
    html_text() |> 
    str_trim()
}

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
                 'location' = 2,
                 'district' = 3
   )
 return(df)
}



get_location(html)

get_prices <- function(html){
  df <- html |>
    html_nodes('#aPreise .cell__row') |> 
    html_text() |> 
    str_trim() |>
    readr::parse_number() |>
    matrix(nrow = 1) |>
    as_tibble() |>
    dplyr::rename('cold_rent' = 1, 
                  'service_charges' = 2,
                  'heating_costs' = 3,
                  'warm_rent' = 4
                  )
  
  return(df)
  
}






# all prices 
html %>%
  html_nodes('#aPreise .card') %>% 
  html_text() %>% 
  str_trim() 
  

html_sub %>%
  html_nodes('#aPreise .card') %>% 
  html_text() %>% 
  str_trim()
  
  
# deposit
html_sub %>%
  html_nodes('#aPreise .ng-star-inserted .card-content') %>% 
  html_text() %>% 
  str_trim() %>%
  readr::parse_number()


html_sub %>%
  html_nodes('.hardfact__label') %>% 
  html_text() %>% 
  str_trim() %>%
  readr::parse_number()

# rent, square meter, rooms
html_sub %>%
  html_nodes('.has-font-300') %>% 
  html_text() %>% 
  str_trim() %>%
  stringr::str_replace_all(',', '.') %>%
  readr::parse_number()

# Energy 

# energy class
html_sub %>%
  html_nodes('.ng-star-inserted:nth-child(5) p') %>% 
  html_text() %>% 
  str_trim() 

# year
html_sub %>%
  html_nodes('.cell:nth-child(3) p') %>% 
  html_text() %>% 
  str_trim() 

# ausstattung
html_sub %>%
  html_nodes('app-details .ng-star-inserted') %>% 
  html_text() %>% 
  str_trim() 








# ------------------------------- done -------------------

# most information
html_sub %>%
  html_nodes('.object-meta') %>% 
  html_text() %>% 
  str_trim()

##--- information by information ----

# PLZ, location, urban district
html_sub %>%
  html_nodes('#exposeAddress div') %>% 
  html_text() %>% 
  str_trim() %>%
  str_split(' ') %>% 
  unlist()









#---------------------------- old (main page)  --------------
# titel 
html %>%
  html_nodes('.EstateItem-1c115 h2 ') %>% 
  html_text() %>% 
  str_trim()
 

# key facts 
html %>%
  html_nodes('.KeyFacts-efbce') %>% 
  html_text() %>% 
  str_trim()



html %>%
  html_nodes('.IconFact-e8a23:nth-child(1) span') %>% 
  html_text() %>% 
  str_trim()


html %>%
  html_nodes('.KeyFacts-efbce:nth-child(1) div') %>% 
  html_text() %>% 
  str_trim()

  
