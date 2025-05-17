library(tidyverse)

page_url <- "https://books.toscrape.com/"
doc <- read_html(page_url)

# 1. Extract titles
titles <- doc %>% 
  html_elements("h3 a") %>% 
  html_attr("title")

# 2. Extract prices and ratings
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
get_data <- function(doc, css, attr = NULL, formatter = NULL) {
  res <- doc %>% 
    html_elements(css) 
  
  if(!is.null(attr)) {
    res <- res %>% html_attr(attr)
  } else {
    res <- res %>% html_text()
  }
  
  if(!is.null(formatter)) {
    res <- purrr::map_dbl(res, formatter)  %>% unlist
  }
  
  return(res)
}





all_books <- list()
i <- 1
repeat {
  page_url <- sprintf("https://books.toscrape.com/catalogue/page-%s.html", i)
  if (httr::status_code(httr::GET(page_url)) != 200 | i == 11) break;
  message(sprintf("Scraping page %s", i))
  doc <- read_html(page_url, options = c("RECOVER"))
  
  titles  <- get_data(doc, css = "h3 a", attr = "title")
  prices  <- get_data(doc, css = ".price_color", formatter = parse_number)
  ratings <- get_data(doc, css = ".star-rating", attr = "class") %>%
    str_extract("One|Two|Three|Four|Five") %>%
    recode("One" = 1, "Two" = 2, "Three" = 3, "Four" = 4, "Five" = 5)
  
  all_books[[i]] <- tibble(title = titles, price = prices, rating = ratings)
  i <- i + 1
}

books_df <- bind_rows(all_books)

ggplot(books_df) +
  geom_point(aes(x = rating, y = price))
