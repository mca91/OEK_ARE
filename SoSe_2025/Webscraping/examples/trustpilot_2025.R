library(tidyverse)
library(rvest)
library(lubridate)

# Utility functions -------------------------------------------------------

get_trimmed_text <- function(html, selector) {
  html %>% html_elements(selector) %>% html_text(trim = TRUE)
}

get_html_attr <- function(html, selector, attr) {
  html %>% html_elements(selector) %>% html_attr(attr)
}

# Scraper functions ------------------------------------------------------

get_review_cards <- function(page_url) {
  read_html(page_url) %>%
    html_elements("article.styles_reviewCard__Qwhpy")
}

extract_page_data <- function(company, page_url) {
  cards <- get_review_cards(page_url)
  
  tibble(
    company = company,
    author  = get_trimmed_text(cards, ".styles_consumerDetails__POC79 > span"),
    date    = get_html_attr(cards, "time", "datetime") %>% ymd_hms(),
    rating  = get_html_attr(cards, ".star-rating_starRating__sdbkn > img", "alt") %>%
      str_extract("\\d") %>% as.integer(),
    title   = get_trimmed_text(cards, ".styles_reviewContent__tuXiN h2"),
    review  = get_trimmed_text(cards, ".styles_reviewContent__tuXiN .typography_body-l__v5JLj") %>% 
      { ifelse(length(.) == 0, "", .) }  # sometimes the review contains only a title
  )
}

# Company-wise scraper ---------------------------------------------------
cmpns <- list(
  "Ebay.com" = "https://www.trustpilot.com/review/ebay.com?page=",
  "Amazon.com" = "https://www.trustpilot.com/review/amazon.com?page="
)

get_company_reviews <- function(company, num_pages) {
  message(company)
  map_dfr(1:num_pages, function(i) {
    message(sprintf("Page %s", i))
    extract_page_data(company, paste0(cmpns[[company]], i))
  })
}

# Main execution ---------------------------------------------------------
df <- map_dfr(names(cmpns), get_company_reviews, num_pages = 20)


# Visualization ----------------------------------------------------------
df %>%
  group_by(company, date = floor_date(date, "day")) %>%
  summarise(N = n(), rating = mean(rating), .groups = "drop") %>%
  ggplot(aes(x = date, y = rating, col = company)) +
  geom_line() +
  geom_point(aes(size = N))
