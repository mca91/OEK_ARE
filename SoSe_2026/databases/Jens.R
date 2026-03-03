
keyring::key_set_with_value(
  service = 'sql_try',
  username = 'sql11460561',
  password = 'D2pkl1XBCf'
)







  
  
  
  
  
  
  group_by(continent) %>% 
  select_if(is.numeric) %>% 
  select(-year) %>%
  summarise_all(mean)