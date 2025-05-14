library(tidyverse)

page_url <- "https://books.toscrape.com/"
doc <- read_html(page_url)

# 1. Extract titles
titles <- doc %>% 
  html_elements("h3 a") %>% 
  html_attr("title")

# 2.
prices <- doc %>% 
  html_elements(".price_color") %>% 
  html_text() %>%
  parse_number()

ratings <- doc %>% 
  html_elements(".star-rating") %>% 
  html_attr("class") %>% 
  str_extract("One|Two|Three|Four|Five") %>%
  recode("One" = 1, "Two" = 2, "Three" = 3, "Four" = 4, "Five" = 5)

df <- tibble(title = titles, price = prices, rating = ratings)
print(df)


# 3.
all_books <- list()
i <- 1
repeat {
  page_url <- sprintf("https://books.toscrape.com/catalogue/page-%s.html", i)
  if (httr::status_code(httr::GET(page_url)) != 200) break;
  message(sprintf("Scraping page %s", i))
  doc <- read_html(page_url, options = c("RECOVER"))
  titles <- doc %>% 
    html_elements("h3 a") %>% 
    html_attr("title")
  
  prices <- doc %>% 
    html_elements(".price_color") %>% 
    html_text() %>%
    parse_number()
  
  ratings <- doc %>% 
    html_elements(".star-rating") %>% 
    html_attr("class") %>% 
    str_extract("One|Two|Three|Four|Five") %>%
    recode("One" = 1, "Two" = 2, "Three" = 3, "Four" = 4, "Five" = 5)
  
  all_books[[i]] <- tibble(title = titles, price = prices, rating = ratings)
  i <- i + 1
}

books_df <- bind_rows(all_books)
