library(tidyverse)
library(data.table)

# reading data 
crime <- readr::read_csv(here::here('SoSe_2022/Rmarkdown/Exercise/crime_boston.csv'))

str(crime)

# reading in description 
crime_description <- readxl::read_xlsx(here::here('SoSe_2022/Rmarkdown/Exercise/rmscrimeincidentfieldexplanation.xlsx'))


# overview 
dplyr::glimpse(crime) # same as str
