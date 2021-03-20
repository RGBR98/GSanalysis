meses <- map2(
  .x = c(2,5,7,9),
  .y = c('oct','nov','dic','ene'),
  .f = ~function(sheet = .x, month = .y){
    df <- read_sheet('https://docs.google.com/spreadsheets/d/1MPzKbtO-_N2hDJONArWLV0Mo8O-l4On1dwU5i7KscD0/edit#gid=1126889633'
                     ,sheet = 5 ) %>% 
      janitor::clean_names() %>% 
      pivot_longer(-c('store','install_date'), names_to = 'issue', values_to = 'number')
    
    return(df)
  }
) %>% 
  bind_rows()

check <- read_sheet('https://docs.google.com/spreadsheets/d/1MPzKbtO-_N2hDJONArWLV0Mo8O-l4On1dwU5i7KscD0/edit#gid=1126889633'
           ,sheet = 5 ) %>% 
  janitor::clean_names() %>% 
  pivot_longer(-c('store','install_date'), names_to = 'issue', values_to = 'number')
