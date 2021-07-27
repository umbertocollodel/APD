# Script to produce figure lockdowns and stringency


countries=c("India", "Indonesia","Philippines","Korea, Republic of", "Myanmar",
            "Thailand", "New Zealand", "Australia", "Japan", 
            "Vietnam","China, People's Republic of")

dates_reopening <- read_xlsx("../APD_material/raw_data/health_stringency_index.xlsx", sheet = 2, skip = 1) %>% 
  gather("date","value",`January 1, 2020`:ncol(.)) %>% 
  mutate(date = as.character(as.Date(date, format = "%B %d, %Y"))) %>% 
  filter(date < "2020-10-10") %>%
  filter(Country %in% countries) %>% 
  group_by(Country) %>% 
  mutate(lag_value = dplyr::lag(value,1)) %>% 
  mutate(dummy = case_when(value != lag_value ~ 1,
                           T ~ 0),
         max = case_when(value == max(value) ~ 1,
                         T ~0),
         lag_max = dplyr::lag(max, 1)) %>% 
  filter(dummy == 1 & lag_max ==1) %>% 
  arrange(date) %>%
  distinct(Country, .keep_all = T) %>% 
  split(.$Country) %>% 
  map(~ .x %>% .$date)

# Death data: -----


deaths_august <- read_xlsx("~/Desktop/ERP matrix.xlsx", sheet = "OWID Data") %>%
  filter(countrycode(location,"country.name","imf") %in% countrycode(countries,"country.name","imf")) %>%
  filter(AlternativeDate == "2020M8")

# Death data: -----

deaths_data <- read_xlsx("~/Desktop/ERP matrix.xlsx", sheet = "new_deaths_per_million") %>% 
  gather("country","deaths",World:ncol(.)) %>% 
  filter(countrycode(country,"country.name","imf") %in% countrycode(countries,"country.name","imf")) %>%
  mutate(country = case_when(country == "South Korea" ~ "Korea",
                             T ~ country)) %>% 
  mutate(date = as.character(date)) %>% 
  split(.$country) 

# Peak deaths before reopening: -----

max_deaths <- deaths_data %>% 
  map2(dates_reopening, ~ .x %>% filter(date <= .y)) %>% 
  map(~ .x %>% slice(which.max(deaths))) %>% 
  bind_rows() %>% 
  rename(max_deaths = deaths) %>% 
  select(country, max_deaths)



# Combine information: ----


deaths_reopening <- deaths_data %>% 
  map2(dates_reopening, ~ .x %>% filter(date %in% .y)) %>% 
  bind_rows() %>% 
  rename(deaths_reopening = deaths) %>% 
  filter(country != "Vietnam")
  

max_deaths %>% 
  merge(deaths_reopening) %>% 
  mutate(change = (deaths_reopening - max_deaths)/max_deaths)



# Plot: ------



deaths_august %>% 
  ggplot(aes(location, NewConfirmedDeaths)) +
  geom_point() +
  theme_minimal()

  

