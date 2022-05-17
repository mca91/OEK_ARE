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
# drooping url
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





  









