library(tidyverse)  
library(rvest)    
library(stringr)   
library(rebus)     
library(lubridate)
library(readr)
library(V8)
library(httr)
library(jsonlite)
library(magrittr)

get_name <- function(html){
  html %>%
    html_nodes('.consumer-information__name') %>%   
    html_text() %>% 
    str_trim()
}



get_date <- function(html){

html %>%  
  html_text %>%
  str_trim() %>% 
  str_match_all('publishedDate\\D+(\\d.+?Z)\\D') %>% data.frame(stringsAsFactors = F) %>%
  extract2(2) %>%
  lubridate::ymd_hms()
 }

get_rating <- function(html){

 html %>%
  html_nodes('.review .star-rating img') %>%  
  map(.x = ., ~ html_attr(.x, "alt")) %>% 
  map(~ str_match(.x, "[1-5]")) %>% 
  as.numeric()
  
}


get_title <- function(html){
  html %>% html_nodes('.link--dark') %>%   
    html_text() %>% 
    str_trim()  %>% 
    unlist
}

get_review <- function(html){
  html %>% html_nodes('.review-content__text') %>%   
    html_text() %>% 
    str_trim() %>% 
    unlist
}


extract_page_data <- function(url){
  html <- read_html(url) 
  tibble(name = get_name(html),
         date = get_date(html),
         rating = get_rating(html), 
         title = get_title(html), 
         review = get_review(html))

}




get_company_data <- function(company, num_pages){
  i <- 1
  repeat{
    if(i == 1){
     df <- extract_page_data(paste0("https://www.trustpilot.com/review/", company, "page=",i))
    } else {
     temp_df <- extract_page_data(paste0("https://www.trustpilot.com/review/", company, "page=",i))
     if(any(duplicated( rbind(df, temp_df))) | i == num_pages ) break
     df <- rbind(df, temp_df)
    }
    print(i)
    i <- i + 1
    Sys.sleep(10)
  }
  return(df)
}

extract_page_data("https://www.trustpilot.com/review/www.amazon.com?page=12")


companies <-setNames(c("www.amazon.com?", "www.alibaba.com?"), 
                     c("Amazon", "Alibaba"))

data <- map2_dfr(companies, c(20, 20), get_company_data, .id = "company")


data %>% 
  group_by(company, date =  floor_date(date, "month")) %>%
  summarise(N = n(), rating = mean(rating)) %>% 
  ggplot(aes(x = date, y = rating, col = company)) + geom_line() + geom_point(aes(size = N)) #+ 
  coord_cartesian(xlim = c(as.POSIXct("2015-01-01"), as.POSIXct("2019-08-01")))



