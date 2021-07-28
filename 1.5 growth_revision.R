########## Script to produce figure revision of GDP growth:


readRDS("~/Dropbox/Forecasts_Time_Covid/Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS") %>% 
  filter(country_code %in% apd_list_countries) %>% 
  filter(horizon == "Jan") %>% 
  mutate(diff = -(Actual - abs(value))) %>% 
  select(country, value, Actual,diff) %>% 
  arrange(-diff) %>% 
  setNames(c("Country","January 2020 Projection","April 2021 Estimate","Difference from Pre-Pandemic Jan. 2020
  World Economic Outlook (abs)")) %>% 
  write.table(file = "../APD_material/output/tables/growth_revision.pdf",
              row.names = F,
              quote = F,
              sep = ",")
