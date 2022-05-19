# packages 
library(tidyverse)  
library(stringr)   
library(lubridate)
library(magrittr)

# load data
load(here::here('SoSe_2022/Webscraping/examples/immo_data.RData'))
load(here::here('SoSe_2022/Webscraping/examples/immo_data_big.RData'))
#

data <- data_592

names(data)
str(data)
# drooping url and building_type
data %<>%
  dplyr::select(-c(url, building_type))

data_essen <- data %>%
  filter(city == 'essen',
         !is.na(warm_rent))  %>%
  dplyr::slice_sample(n = 150)

data_bochum <- data %>%
  filter(city == 'bochum', 
         !is.na(warm_rent)) %>%
  dplyr::slice_sample(n = 150)

data <- data_essen %>%
  bind_rows(data_bochum)
  

######################################################################################
#################################### actual tasks ####################################
######################################################################################

# changing format 
data %<>%
  dplyr::mutate(zipcode = factor(zipcode), 
                city = factor(city),
                building_year = as.numeric(building_year),
                efficiency_class = factor(efficiency_class, levels = c('A+', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'))
  )

# sqr meter

data %<>%
  dplyr::mutate(cold_rent_per_sqm = cold_rent / square_meter,
                warm_rent_per_sqm = warm_rent / square_meter)


# Create one table with the five districts with the most ads for each city. 
data %>%
  group_by(city, zipcode) %>%
  summarise(N = n()) %>%
  group_by(city) %>%
  slice_max(N, n = 5, with_ties = FALSE)


top_5_zipcodes <- data %>%
  group_by(city, zipcode) %>%
  summarise(N = n()) %>%
  group_by(city) %>%
  slice_max(N, n = 5, with_ties = FALSE) %>%
  pull(zipcode)


# boxplot 

#data_zip <- 

data %>%
  filter(zipcode %in% top_5_zipcodes) %>%
  ggplot2::ggplot(aes(x = zipcode, y = warm_rent, fill = city)) +
  geom_boxplot() 
# scatterplot 

data %>%
  tidyr::pivot_longer(cols = c(cold_rent_per_sqm, warm_rent_per_sqm), names_to = 'class', values_to = 'value') %>%
  ggplot2::ggplot(aes(x = square_meter, y = value, color = city, shape = city)) +
  ggplot2::geom_point(alpha = 0.5) +
  ggplot2::geom_smooth(aes(group = 1), method = lm, color = '#878787') +
  ggplot2::facet_wrap(~class) + 
  theme_minimal()

# boxplot cold rent and warm rent wann was? 
data %>%
  tidyr::pivot_longer(cols = c(cold_rent, warm_rent), names_to = 'class', values_to = 'value') %>%
  ggplot2::ggplot(aes(x = city, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~class)

data %>%
  tidyr::pivot_longer(cols = c(cold_rent_per_sqm, warm_rent_per_sqm), names_to = 'class', values_to = 'value') %>%
  ggplot2::ggplot(aes(x = city, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~class)


# boxplot efficiency classes cold rents   

data %>%
  tidyr::pivot_longer(cols = c(cold_rent_per_sqm, warm_rent_per_sqm), names_to = 'class', values_to = 'value') %>%
  ggplot2::ggplot(aes(x = efficiency_class, y = value)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~class)


data %>%
  ggplot2::ggplot(aes(x = efficiency_class, y = energy_demand)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(~class)


# regressions

str(data)
data_lm <- data %>%
  select(where(is.numeric)) 

lm(cold_rent ~ zipcode + efficiency_class + rooms + building_year + square_meter, data_lm)

# sample 200 
# City vs zipcode

summary(lm(cold_rent ~ zipcode  + efficiency_class + rooms + building_year + square_meter + service_charges, data_lm))
summary(lm(cold_rent ~ city + efficiency_class + rooms + building_year + square_meter + service_charges, data_lm))



### --- sql data bank 
appointments <- readr::read_csv(here::here('SoSe_2022/datasets/appointments.csv'))


str(appointments)
class(appointments$scheduled)

library(dplyr)
con <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = here::here('SoSe_2022/databases',"assignment_1.sqlite3"), 
)

DBI::dbWriteTable(con,
                  name = "appointments", 
                  value = appointments)

DBI::dbListTables(con)


DBI::dbDisconnect(con)

rm(con)


library(dplyr)
con_2 <- DBI::dbConnect(
  drv = RSQLite::SQLite(),
  dbname = "C:/Users/jens.klenke/Downloads/assignment_1.sqlite3", 
)

DBI::dbListTables(con_2)


appointments_db <- dplyr::tbl(con_2, 'appointments')

show_query(appointments_db)

table <- collect(appointments_db, n = Inf)
 

appointments_1 <- dplyr::tbl(con, 'appointments')

table_1 <- collect(appointments_1, n = Inf)

identical(table_1, table)



#### try new data ####

credit_card <- readxl::read_xls(here::here('SoSe_2022/datasets/default_of_credit_card_clients.xls'))

names(credit_card)


credit_card %>%
  distinct(PAY_0)

library(ggplot2)
library(lubridate)
#### tram ####

metro <- readr::read_csv(here::here('SoSe_2022/datasets/Metro_Interstate_Traffic_Volume.csv'))

names(metro)

str(metro)
class(metro$date_time)


### database  
  
library(dplyr)
  con <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = here::here('SoSe_2022/databases/Metro_Interstate_Traffic_Volume.sqlite3'), 
  )  

  DBI::dbWriteTable(con,
                    name = "metro", 
                    value = metro)
  str(metro)
  str(metro_db)
# table connection 
  
metro_db <- tbl(con, 'metro')
  
# collect data 

metro_db <- tbl(con, 'metro') %>%
  collect(n = Inf)


str(metro_db)

metro_db %>%
  distinct(weather_main)      

## compute weekdays

#functions
# own function for theme 
# function for theme 

day_levels = c('Monday', 'Tuesday', 'Wednesday', 
               'Thursday', 'Friday', 'Saturday', 'Sunday')   

try <- metro_db %>%
  dplyr::mutate(date_time = as_datetime(date_time),
                weekday = factor(weekdays(date_time), 
                                 levels = day_levels))

try %>%
ggplot2::ggplot(aes(x = weekday, y = traffic_volume)) +
  ggplot2::geom_boxplot()


# test

# lineplot 
metro %>%
  dplyr::filter(date_time >= lubridate::ymd_hms('2014-01-01 00:00:00') & date_time <= lubridate::ymd_hms('2015-12-31 24:00:00')) %>%
  ggplot(aes(x = date_time, y = traffic_volume)) +
  geom_line(size = 0.000000000001, alpha = 0.2)

# Jahres vergleich



