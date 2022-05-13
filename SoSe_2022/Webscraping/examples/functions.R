########## functions


extract_links <- function(url){
  
  tibble(url = read_html(url) %>%
           html_elements('.noProject-eaed4') %>%
           html_attr('href')
  )
}

get_links<- function(locations, num_pages){
  i <- 1
  repeat{
    if(i == 1){
      df <- extract_links(paste0('https://www.immowelt.de/liste/' , locations,
                                 '/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=', i))
      print(paste(locations, i))
    } else {
      temp_df <- extract_links(paste0('https://www.immowelt.de/liste/' , locations,
                                      '/wohnungen/mieten?d=true&sd=DESC&sf=RELEVANCE&sp=', i))
      if(any(duplicated(rbind(df, temp_df)))) break
      df <- rbind(df, temp_df)
      print(paste(locations, i))
      if(i == num_pages) break
    }
    i <- i + 1
    Sys.sleep(sample(2:10, 1))
  }
  return(df)
}

# title 
get_title <- function(html){
  html |>
    html_nodes('h1') %>% 
    html_text() %>%
    str_trim()
}

### locations (zipcode)
get_location <- function(html){
  df <- html %>%
    html_nodes('#exposeAddress div') %>%
    html_text() %>%
    str_trim() %>%
    str_split(' ') %>%
    unlist() %>%
    str_replace_all("\\(|\\)", "") %>%
    matrix(nrow = 1) %>%
    as_tibble() %>%
    dplyr::rename('zipcode' = 1, 
                  'city' = 2,
                  'district' = 3
    )
  return(df)
}


# alle mieten
get_rents <- function(html){
  
  tibble(class = c('Kaltmiete', 'Nebenkosten', 'Heizkosten in Warmmiete enthalten', 
                   'Heizkosten nicht in Warmmiete enthalten', 'Warmmiete')) %>%
    left_join(html %>%
                html_nodes('#aPreise .cell__row') %>% 
                html_text() %>%
                str_trim() %>%
                as_tibble() %>%
                separate(value, into = c('class', 'value'), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
              , by = 'class'
    ) %>%
    dplyr::mutate(value = readr::parse_number(value, 
                                              locale = locale(decimal_mark = ",", 
                                                              grouping_mark = "."))) %>%
    pivot_wider(names_from = class, values_from = value) %>%
    dplyr::rename(
      'cold_rent' = 'Kaltmiete',
      'service_charges' = 'Nebenkosten',
      'heating_cost_included' = 'Heizkosten in Warmmiete enthalten',
      'heating_cost_excluded' = 'Heizkosten nicht in Warmmiete enthalten',
      'warm_rent' = 'Warmmiete') # Heizkosten in oder nicht in Warmmiete
}

get_cold.rent <- function(html){
  suppressWarnings(
  html %>%  
    html_nodes('#aPreise .cell__row') %>% 
    html_text() %>%
    str_trim() %>%
    as_tibble() %>%
    separate(value, into = c('class', 'value'), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
    dplyr::filter(str_detect(class, 'Kaltmiete')) %>%
    dplyr::mutate(value = readr::parse_number(value, 
                                            locale = locale(decimal_mark = ",", 
                                                            grouping_mark = "."))) %>%
    dplyr::pull(value)) %>%
    ifelse(is_empty(.), NA, .)
}

get_warm.rent <- function(html){
  suppressWarnings(
    html %>%  
      html_nodes('#aPreise .cell__row') %>% 
      html_text() %>%
      str_trim() %>%
      as_tibble() %>%
      separate(value, into = c('class', 'value'), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
      dplyr::filter(str_detect(class, 'Warmmiete') & !str_detect(class, 'Heizkosten')) %>%
      dplyr::mutate(value = readr::parse_number(value, 
                                                locale = locale(decimal_mark = ",", 
                                                                grouping_mark = "."))) %>%
      dplyr::pull(value)) %>%
    ifelse(is_empty(.), NA, .)
}

get_service.charges <- function(html){
  suppressWarnings(
    html %>%  
      html_nodes('#aPreise .cell__row') %>% 
      html_text() %>%
      str_trim() %>%
      as_tibble() %>%
      separate(value, into = c('class', 'value'), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
      dplyr::filter(str_detect(class, 'Nebenkosten')) %>%
      dplyr::mutate(value = readr::parse_number(value, 
                                                locale = locale(decimal_mark = ",", 
                                                                grouping_mark = "."))) %>%
      dplyr::pull(value)) %>%
    ifelse(is_empty(.), NA, .)
}

get_heating.cost.included <- function(html){
  suppressWarnings(
    html %>%  
      html_nodes('#aPreise .cell__row') %>% 
      html_text() %>%
      str_trim() %>%
      as_tibble() %>%
      separate(value, into = c('class', 'value'), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
      dplyr::filter(str_detect(class, 'Heizkosten in Warmmiete enthalten')) %>%
      dplyr::mutate(value = readr::parse_number(value, 
                                                locale = locale(decimal_mark = ",", 
                                                                grouping_mark = "."))) %>%
      dplyr::pull(value)) %>%
    ifelse(is_empty(.), NA, .)
}

get_heating.cost.excluded <- function(html){
  suppressWarnings(
    
    html %>%  
      html_nodes('#aPreise .cell__row') %>% 
      html_text() %>%
      str_trim() %>%
      as_tibble() %>%
      separate(value, into = c('class', 'value'), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
      dplyr::filter(str_detect(class, 'Heizkosten nicht in Warmmiete enthalten')) %>%
      dplyr::mutate(value = readr::parse_number(value, 
                                                locale = locale(decimal_mark = ",", 
                                                                grouping_mark = "."))) %>%
      dplyr::pull(value) %>%
      ifelse(is_empty(.), NA, .)
    )
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

# deposit
get_deposit <- function(html){
  html %>%
    html_nodes('#aPreise .ng-star-inserted .card-content') %>% 
    html_text() %>% 
    str_trim() %>%
    readr::parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))
}

get_energy.information <- function(html){
 tibble(
    class = c('Gebäudetyp', 'Baujahr laut Energieausweis',
              'Effizienzklasse', 'Endenergiebedarf', 'Endenergieverbrauch')) %>%
    left_join(
      tibble(
      class = html %>%
        html_nodes('.energy_information .has-font-75') %>%
        html_text() %>%
        str_trim(),
      
      value = html %>%
        html_nodes('.energy_information .has-font-75+ p') %>% 
        html_text() %>% 
        str_trim()
      ),
      by = 'class') %>%
    pivot_wider(names_from = class, values_from = value) %>%
    dplyr::mutate(energy_demand = case_when(
      is.na(Endenergiebedarf) & !is.na(Endenergieverbrauch) ~ Endenergieverbrauch,
      !is.na(Endenergiebedarf) &  is.na(Endenergieverbrauch) ~ Endenergiebedarf,
      TRUE ~ NA_character_ 
      )) %>%
    dplyr::select(-c(Endenergiebedarf, Endenergieverbrauch)) %>%
    dplyr::mutate(energy_demand = readr::parse_number(energy_demand,
                                                         locale = locale(decimal_mark = ",", 
                                                                         grouping_mark = "."))) %>%
    dplyr::rename('building_type' = Gebäudetyp,
                  'building_year' = `Baujahr laut Energieausweis`, 
                  'efficiency_class' = Effizienzklasse) 
  
}




# extracting data per side 
extract_page_data <- function(url, row_num, ...){ #  row_num,
  html <- read_html(url) 
  df <-  tibble(title = get_title(html),
                get_location(html),
                cold_rent = get_cold.rent(html),
                heating_cost_excluded = get_heating.cost.excluded(html),
                heating_cost_included = get_heating.cost.included(html),
                service_charges = get_service.charges(html),
                warm_rent = get_warm.rent(html),
                deposite = get_deposit(html), 
                get_size(html) ,
                get_energy.information(html),
                url = url
  )
  print(paste(row_num, "of", nrow(data)))
  
  Sys.sleep(sample(2:7, 1))
  return(df)
}


