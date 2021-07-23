# GDP: -----

ngdp <- read_xlsx("~/Dropbox/Emergency_response_covid/Emergency_response_covid_material/raw_data/WEOOct2020.xlsx") %>% 
  select(Country,`2019`:`2020`) %>% 
  mutate_at(vars(matches("\\d")),as.numeric) %>% 
  mutate_at(vars(matches("\\d")),funs(.*1000)) %>% 
  gather("year","ngdp_usd",`2019`:ncol(.)) %>% 
  rename(country = Country) %>% 
  filter(year == 2019) %>% 
  mutate(iso3c = countrycode(country,"country.name","iso3c")) %>%
  mutate(iso3c = case_when(country == "Kosovo" ~ "KSV",
                           country == "Eswatini" ~ "SWZ",
                           T ~ iso3c)) %>% 
  mutate(country.code = countrycode(iso3c,"iso3c","imf")) %>% 
  select(-year, -country) %>% 
  filter(complete.cases(.))

# Income group: -----

income_group < -read_xlsx("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/country_group.xlsx") %>% 
  filter(ifscode %in% apd_list_countries) %>%
  rename(country.code = ifscode) %>%
  mutate(group = case_when(adv == 1 ~ "Advanced",
                           eme == 1 & lidc == 0 ~ "EM",
                           eme == 1 & lidc == 1 ~ "LIDC")) %>% 
  select(country.code, group)
  



# Governement support figures: ------


read_xlsx("~/Desktop/ADB_Tracker.xlsx") %>%
  mutate(country.code = countrycode(Economy,"country.name","imf")) %>%
  mutate(Measure = case_when(row_number() < 79 ~ "Liquidity Support",
                             row_number() >= 80 & row_number() < 159 ~ "Credit Creation",
                             row_number() >= 159 & row_number() < 238 ~ "Direct Long-Term Lending",
                             row_number() >= 238 & row_number() < 317 ~ "Equity Support",
                             row_number() >= 317 & row_number() < 396 ~ "Health and Income support",
                             row_number() >= 396 & row_number() < 475 ~ "Budget Reallocation",
                             row_number() >= 475 & row_number() < 554 ~ "Central Bank financing government",
                             row_number() >= 554 & row_number() <= 632 ~ "No breakdown")) %>% 
  filter(country.code %in% apd_list_countries) %>% 
  gather("date","value",`Apr 20, 2020`:`Mar 22, 2021`) %>% 
  filter(date == "Mar 22, 2021") %>% 
  group_by(country.code) %>% 
  summarise(total = sum(value, na.rm = T)/1000000) %>%
  merge(income_group, by=c("country.code")) %>%
  merge(ngdp, by=(c("country.code"))) %>% 
  mutate(`value/gdp` = total/ngdp_usd *100) %>% 
  ungroup() %>% 
  group_by(group) %>% 
  summarise(median = median(`value/gdp`))
  arrange(-total) %>% 
  print(n=Inf)
  unique() %>% 
  sort()