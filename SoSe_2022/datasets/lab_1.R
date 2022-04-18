##---- packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, 
               knitr,
               stringr,
               here,
               pdftools,
               utils,
               stringi, 
               readr,
               readxl, 
               Hmisc,
               data.table,
               purrr,
               janitor,
               magrittr,
               lubridate
)

### read data
load(here::here('SoSe_2022/datasets/sells_data.RData'))

# overview 
str(sells_data)
head(sells_data) # date format mdy


#### cleaning column names
# first version 
sells_data %>%
  rename_with(~tolower(gsub(" ", "_", .x, fixed = TRUE)))

# also discuss how you can use variable Names with withespaces (backtickels ``)

sells_data %<>%
  janitor::clean_names()



##---- data wrangling ----##

sells_data %<>%
  # computing 
  dplyr::mutate(order_date = lubridate::mdy(order_date),
         ship_date = lubridate::mdy(ship_date),
         process_time = ship_date - order_date)

# cookbook 




# ggplot 
# boxplot of time process per delivery
sells_data %>%
  dplyr::mutate(ship_mode = factor(ship_mode, c('Same Day', 'First Class',
                                                'Second Class', 'Standard Class'))) %>%
  ggplot2::ggplot( aes(x = ship_mode, y = process_time)) +
  ggplot2::geom_boxplot()



# histogramm of orders per day
sells_data %>%
  dplyr::mutate(weekday = factor(weekdays(order_date), weekdays(min(BBBB$order_date) + 3:9))) %>%
  ggplot2::ggplot( aes(x= weekday)) +
  ggplot2::geom_histogram(stat = 'count') 


