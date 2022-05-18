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
