library(gsheet)
library(googlesheets4)
library(tidyverse)
library(gt)
library(glue)
library(dbplyr)
library(patchwork)
issues <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1MPzKbtO-_N2hDJONArWLV0Mo8O-l4On1dwU5i7KscD0/edit#gid=0') #HTML not the actual sheet
issue <- read_sheet('docs.google.com/spreadsheets/d/1MPzKbtO-_N2hDJONArWLV0Mo8O-l4On1dwU5i7KscD0/edit#gid=0') #The actual sheet
oct_data <- read_sheet('https://docs.google.com/spreadsheets/d/1MPzKbtO-_N2hDJONArWLV0Mo8O-l4On1dwU5i7KscD0/edit#gid=1126889633'
                       ,sheet = 5 ) 


aux <- oct_data %>% 
  na.omit() %>% 
  janitor::clean_names() %>% 
  rename('Adapter' = adapter_issues,
         'BSOD' = blue_screen_of_death,
         'Dead USB ports' = dead_usb_ports,
         'Dock not recognizing peripherals' = dock_issues_not_recognizing_peripherals,
         'Frozen POS' = frozen_tablet,
         'Inventory missing' = inventory_issues,
         'Label printer' = label_printer,
         'Network' = network_issues,
         'No issues' = no_issues,
         'Pinpad' = pinpad,
         'Polling' = polling,
         'No power' = power_issues_dock_tablet_and_devices,
         'Receipt printer' = receipt_printer,
         'Scanner' = scanner,
         'Slow register' = slow_register,
         'Stuck payment' = stuck_payment,
         'Replaced'= replaced,
         'GV' = gv)%>% 
  
  select(-c('store', 'updated_on','total_sum_of_issues_per_store')) %>%
  mutate(GV = unlist(GV),
         Replaced = unlist(Replaced),
         install_date = 'November') 
total <- read_csv("Data/total.csv") %>% 
  bind_rows(aux)
total %>% write_csv('Data/total.csv')

pivot_longer(-install_date, names_to = 'issues',values_to = 'count') 





mark


total <- read_csv('Data/total.csv') 

total %>%   
  ggplot() +
  ggtitle('October')+
  aes(x = 1, y = count, fill = issues)+
  geom_bar(stat = 'identity', position = 'stack',color ='black', width = 10000)+
  theme_void()+
  coord_flip()+
  scale_fill_manual(values = c('#27AD23','#F8F1F0','#7B33FF','#EFBDFF','#F2C25A','#9AB0EB','#3EA2F0','#E06C5A',
                               '#3EF0D5','#F7C30C','#E01001','#3AF04C','#22F0B6','#FAF09B','#3A5FF0','#F0223A',
                               '#8EF022','#239E48'))+
  coord_polar("y", start=0)+
  
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.position = 'bottom',
    plot.title = element_text(color="black", size=20, face="bold.italic"))





