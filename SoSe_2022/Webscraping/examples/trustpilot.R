library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)
library(magrittr)

get_name <- function(html){
  html %>%
    html_nodes('.consumer-information__name') %>%   
    html_text() %>% 
    str_trim()
}

get_title <- function(html){
  html %>% html_nodes('.link--dark') %>%   
    html_text() %>% 
    str_trim()
}


get_review <- function(html){
  html %>% html_nodes('.review-content__text') %>%   
    html_text() %>% 
    str_trim() 
}


html %>% 
  html_nodes(".time")


get_date <- function(html){

html %>%  
  html_text %>%
  str_trim() %>% 
  str_match_all('publishedDate\\D+(\\d.+?Z)\\D') %>% 
    data.frame(stringsAsFactors = F) %>%
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
     df <- extract_page_data(paste0("https://www.trustpilot.com/review/", company, "?page=", i))
     print(paste(company, i))
    } else {
     temp_df <- extract_page_data(paste0("https://www.trustpilot.com/review/", company, "?page=",i))
     if(any(duplicated(rbind(df, temp_df)))) break
     df <- rbind(df, temp_df)
     print(paste(company, i))
     if(i == num_pages) break
    }
    i <- i + 1
    Sys.sleep(sample(2:10, 1))
  }
  return(df)
}

# Scrape data for multiple companies using purrr

companies <-setNames(c("www.amazon.com", "www.alibaba.com"), 
                     c("Amazon", "Alibaba"))

# The second argument specifies how many pages are scraped for each company. 
data <- map2_dfr(companies, c(3, 3), get_company_data, .id = "company") 

data %>% 
  group_by(company, date =  floor_date(date, "month")) %>%
  summarise(N = n(), rating = mean(rating)) %>% 
  ggplot(aes(x = date, y = rating, col = company)) + geom_line() + geom_point(aes(size = N)) #+ 
  coord_cartesian(xlim = c(as.POSIXct("2015-01-01"), as.POSIXct("2019-08-01")))

  
  
##### ----- new functions -----
### CSS selectros have change
  
  
  own_url <- paste0("https://www.trustpilot.com/review/", companies[1], "?page=", 1)
  html <- read_html(own_url) 
  names_try <-   get_name(html)
  title_try <- get_title(html)
  review_try <- get_review(html)
  date_try <- get_date(html)

get_name <- function(html){
  html %>%
    html_nodes('.styles_consumerName__dP8Um') %>%   
    html_text() %>% 
    str_trim()
}  

get_title <- function(html){
  html %>% html_nodes('.styles_linkwrapper__73Tdy') %>%   
    html_text() %>% 
    str_trim()
}
  
# problem when empty!
get_review <- function(html){
  html %>% html_nodes('.styles_reviewCard__hcAvl .typography_body__9UBeQ') %>%   
    html_text() %>% 
    str_trim() 
}  

get_date <- function(html){
  
  html %>%  
    html_text %>%
    str_trim() %>% 
    str_match_all('publishedDate\\D+(\\d.+?Z)\\D') %>% 
    data.frame(stringsAsFactors = F) %>%
    extract2(2) %>%
    lubridate::ymd_hms()
}  
  
get_rating <- function(html){
  html %>%
    html_nodes('.styles_reviewHeader__iU9Px img') %>%  
    map(.x = ., ~ html_attr(.x, "alt")) %>% 
    map(~ str_match(.x, "[1-5]")) %>% 
    as.numeric()
}
