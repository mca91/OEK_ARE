library(tidyverse)  
library(rvest)    
library(stringr)   
library(lubridate)
library(magrittr)

#### functions ####

# Author 
get_name <- function(html){
  html %>%
    html_nodes('.styles_consumerName__dP8Um') %>%   
    html_text() %>% 
    str_trim()
} 

# Title of the review
get_title <- function(html){
  html %>% html_nodes('.styles_linkwrapper__73Tdy') %>%   
    html_text() %>% 
    str_trim()
}

# Review
get_review <- function(html){
  html %>% html_nodes('.styles_reviewCard__hcAvl .typography_body__9UBeQ') %>%   
    html_text() %>% 
    str_trim() 
}  

# Published date
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

# extract data
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
    Sys.sleep(sample(2:7, 1))
  }
  return(df)
}

# https://www.trustpilot.com/review/ebay.com
companies <-setNames(c("www.amazon.com", "www.ebay.com"), 
                     c("Amazon", "ebay"))


# The second argument specifies how many pages are scraped for each company. 
data <- map2_dfr(companies, c(2, 2), get_company_data, .id = "company") 

#
data %>% 
  group_by(company, date =  floor_date(date, "day")) %>%
  summarise(N = n(), rating = mean(rating)) %>% 
  ggplot(aes(x = date, y = rating, col = company)) + geom_line() + geom_point(aes(size = N)) 
