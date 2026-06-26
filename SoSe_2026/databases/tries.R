
library(tidyverse)
library(dbplyr)
library(pool)
library(RSQLite)


db_path <- file.path(here::here("SoSe_2026/databases"), "local_db.sqlite")
pool <- pool::dbPool(drv = RSQLite::SQLite(), db = db_path)
DBI::dbWriteTable(pool, "tbl_gapminder", gapminder::gapminder)


new_row <- data.frame(country = "Gondor", continent = "Middleearth", 
                      year = 1123, lifeExp = 42, pop = 422156, gdpPercap = 121)
DBI::dbWriteTable(pool, "tbl_gapminder", value = new_row, append = T)


DBI::dbWriteTable(pool, "tbl_country_codes", gapminder::country_codes)



query <- tbl(pool, "tbl_gapminder") %>%
  filter(year == 2007, lifeExp > 80) 
show_query(query)

collect(query)


gap_avg_db_old <- gap_db %>%
  group_by(continent, year) %>% 
  summarise(meanGdpPerCap = mean(gdpPercap)) %>%
  filter(year == 2002)


tbl(pool, "tbl_gapminder") %>%
  left_join(tbl(pool, "tbl_country_codes"), by = "country") %>%
  filter(year == 2007) %>%
  select(country, year, iso_alpha)


pool::poolClose(pool)
