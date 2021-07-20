########## Script to plot stringency index:


read_xlsx("../APD_material/raw_data/health_stringency_index.xlsx", sheet = 2, skip = 1) %>% 
  gather("date","stringency_index",2:ncol(.)) %>% 
  mutate(Country = case_when(Country == "China, People's Republic of" ~ "China",
                             Country == "Hong Kong Special Administrative Region, People's Republic of China" ~ "Hong Kong",
                             T ~ Country)) %>% 
  mutate(country.code = countrycode(Country,"country.name","imf")) %>% 
  filter(country.code %in% apd_list_countries) %>%
  mutate(date = str_remove(date, ",")) %>% 
  mutate(date = as.Date(date, tryFormats = c("%B %d %Y"))) %>%
  mutate(month = month(date)) %>% 
  group_by(Country, month) %>% 
  summarise(stringency_index = mean(stringency_index, na.rm = T)) %>% 
  filter(Country != "China") %>%
  filter(Country == "Australia" | Country == "India" | Country == "Indonesia") %>% 
  ggplot(aes(month, stringency_index, group = Country,col = Country)) +
  geom_line() +
  theme_minimal()

