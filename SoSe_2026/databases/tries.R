x <- 3
square_1 <- function() x^2
square_1()


x <- 3
square_2 <- function(){ 
  x   <- 2
  foo <- function() x^2
  foo()
}
square_2()


library(dplyr)
library(dbplyr)
library(magrittr)

gap_db <- gapminder::gapminder


gap_avg_db <- 
  
  
  gap_db %>%
  group_by(continent) %>%
  select(where(is.numeric), -year) %>%
  summarise(across(everything(),mean))




# database try
library(RSQLite)

con <- DBI::dbConnect(drv = RSQLite::SQLite(), db = r"{D:/Github/OEK_ARE/SoSq_2026/databases/local_db.sqlite}")
DBI::dbWriteTable(con, "gapminder", gapminder::gapminder)
DBI::dbWriteTable(con, "country_codes", gapminder::country_codes)

gap_db <- tbl(con, "gapminder")  # funktioniert nicht 

gap_db_2 <- tbl(con, "gapminder") %>% 
  collect(n = Inf)


gap_avg_db <- gap_db %>%
  group_by(continent) %>%
  select(where(is.numeric), -year) %>%
  summarise(across(everything(), mean))


gap_avg_db_old <- gap_db %>%
  group_by(continent) %>% 
  select_if(is.numeric) %>% 
  select(-year) %>%
  summarise_all(mean)



gap_avg_db <- gap_db %>%
  group_by(continent) %>%
  select(lifeExp, pop, gdpPercap) %>%
  summarise(across(everything(), mean))

show_query(gap_avg_db)

show_query(gap_avg_db_old)


