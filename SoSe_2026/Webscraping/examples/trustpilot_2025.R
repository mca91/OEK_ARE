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
    date    = get_html_attr(cards, ".styles_reviewCardInnerHeader__8Xqy8 time", "datetime") %>% ymd_hms(),
    rating  = get_html_attr(cards, ".star-rating_starRating__sdbkn > img", "alt") %>%
      str_extract("\\d") %>% as.integer(),
    title   = get_trimmed_text(cards, ".styles_reviewContent__tuXiN h2"),
    review  = get_trimmed_text(cards, ".styles_reviewContent__tuXiN .typography_body-l__v5JLj") %>% 
      { ifelse(length(.) == 0, "", .) }  # sometimes the review contains only a title
  )
}




all_reviews <- list()
i <- 25
repeat {
  page_url <- sprintf("https://www.trustpilot.com/review/www.shein.com?page=%s", i)
  if (i > 25) break;
  message(sprintf("Scraping page %s", i))
  
  rvs <- extract_page_data("Shein.com", paste0(page_url, i))
  
  
  all_reviews[[i]] <- rvs
  i <- i + 1
  Sys.sleep(sample(3:8, 1))
}
reviews_df <- bind_rows(all_reviews)

low_rated <- reviews_df %>% filter(rating <= 2)

library(tidytext)
data("stop_words")
complaints <- low_rated %>% 
  unnest_tokens(word, review) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z]+$")) %>%
  count(word, sort = T)


top_complaints <- complaints %>%
  slice_max(n, n = 15)


ggplot(top_complaints, aes(fct_reorder(word, n), y = n)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Most Common Words in 1-2 Star Reviews", x = "Word", y = "Frequency") +
  theme_minimal()





# Main execution ---------------------------------------------------------
df <- map_dfr(names(cmpns), get_company_reviews, num_pages = 100)


# Visualization ----------------------------------------------------------
df %>%
  group_by(company, date = floor_date(date, "day")) %>%
  summarise(N = n(), rating = mean(rating), .groups = "drop") %>%
  ggplot(aes(x = date, y = rating, col = company)) +
  geom_line() +
  geom_point(aes(size = N))




