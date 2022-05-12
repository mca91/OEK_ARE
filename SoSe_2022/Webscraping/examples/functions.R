########## functions 
# title 
get_title <- function(html){
  html |>
    html_nodes('h1') |> 
    html_text() |> 
    str_trim()
}

### locations (zipcode)
get_location <- function(html){
  df <- html |>
    html_nodes('#exposeAddress div') |> 
    html_text() |> 
    str_trim() |>
    str_split(' ') |> 
    unlist() |>
    str_replace_all("\\(|\\)", "") |>
    matrix(nrow = 1) |>
    as_tibble() |>
    dplyr::rename('zipcode' = 1, 
                  'city' = 2,
                  'district' = 3
    )
  return(df)
}


# Warmmiete
get_warm.rent <- function(html){
  
  df <- html %>%
    html_nodes('#aPreise .cell__row') %>% 
    html_text() %>%
    str_trim() %>%
    as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, 'Warmmiete') & !stringr::str_detect(value, 'Heizkosten')) %>%
    pull() 
  
  ifelse(length(df) == 0, NA, 
         readr::parse_number(df, locale = locale(decimal_mark = ",", grouping_mark = ".")))
  
}


get_cold.rent <- function(html){
  
  df <- html %>%
    html_nodes('#aPreise .cell__row') %>%  #.cell__row:nth-child(4)
    html_text() %>%
    str_trim() %>%
    as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, 'Kaltmiete')) %>%
    pull() 
  
  ifelse(length(df) == 0, NA, 
         readr::parse_number(df, locale = locale(decimal_mark = ",", grouping_mark = ".")))
}

# rent, square meter, rooms
get_size <- function(html){
  html %>%
    html_nodes('.has-font-300') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",")) %>%
    matrix(nrow = 1) %>%
    as_tibble() %>%
    dplyr::select(-1) %>%
    dplyr::rename('square_meter' = 1, 
                  'rooms' = 2
    )
}

# get energy class
get_energy.class <- function(html){
  html %>%
    html_nodes('.ng-star-inserted:nth-child(5) p') %>% 
    html_text() %>% 
    str_trim() %>%
    .[2]
}

# energy consumption
get_energy.cons <- function(html){
  html %>%
    html_nodes('.ng-star-inserted:nth-child(6) .has-font-75+ p') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
}

# build year
get_year <- function(html){
  html %>%
    html_nodes('app-energy .cell:nth-child(3) p') %>% 
    html_text() %>% 
    str_trim() %>%
    .[2]
}

# deposit
get_deposit <- function(html){
  html %>%
    html_nodes('#aPreise .ng-star-inserted .card-content') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
}

# Building type
get_building.type <- function(html){
  html %>%
    html_nodes('.energy_information .ng-star-inserted:nth-child(2) .has-font-75+ p') %>% 
    html_text() %>% 
    str_trim()
}



