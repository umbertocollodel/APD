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


cases_august <- read.csv("../APD_material/raw_data/total-and-daily-covid-cases-per-million.csv") %>%
  filter(countrycode(Entity,"country.name","imf") %in% countrycode(countries,"country.name","imf")) %>%
  filter(str_detect(Day, "2020-08")) %>% 
  group_by(Entity) %>% 
  setNames(c("country","iso","date","total","confirmed")) %>% 
  summarise(avg_cases = mean(confirmed, na.rm = T))

# Death data: -----

cases_data <- read.csv("../APD_material/raw_data/total-and-daily-covid-cases-per-million.csv") %>%
  filter(countrycode(Entity,"country.name","imf") %in% countrycode(countries,"country.name","imf")) %>%
  setNames(c("country","iso","date","total","confirmed")) %>% 
  mutate(country = as.character(country), date = as.character(date), iso = as.character(iso)) %>% 
  mutate(country = case_when(country == "South Korea" ~ "Korea",
                             T ~ country)) %>% 
  split(.$country) %>% 
  discard(~ nrow(.x) == 0)

# Peak deaths before reopening: -----

max_cases <- cases_data %>% 
  map2(dates_reopening, ~ .x %>% filter(date <= .y)) %>% 
  map(~ .x %>% slice(which.max(confirmed))) %>% 
  bind_rows() %>% 
  rename(max_cases = confirmed) %>% 
  select(country, max_cases)



# Combine information: ----


cases_reopening <- cases_data %>% 
  map2(dates_reopening, ~ .x %>% filter(date %in% .y)) %>% 
  bind_rows() %>% 
  rename(cases_reopening = confirmed) 


df <- max_cases %>% 
  merge(cases_reopening) %>% 
  mutate(change = (cases_reopening - max_cases)/max_cases*100) %>% 
  as_tibble() %>% 
  merge(cases_august) %>% 
  mutate(opening = case_when(country %in% c("India","Indonesia","Philippines") ~ "Opened before flattening",
                             country %in% c("Australia","Japan") ~ "Second wave",
                             T ~ "Opened following flattening")) %>% 
  mutate(country = fct_reorder(country, opening)) 


# Plot: ------


df %>% 
  ggplot(aes(country)) +
  geom_col(aes(y=change, fill = opening), width = 0.7, alpha = 0.9) +
  geom_point(aes(y=-avg_cases), col = "grey", size=  3.5) +
  scale_y_continuous("Cases at reopening relative to peak before (% change)", sec.axis = sec_axis(~ .,"New cases per million, August (reversed)")) +
  xlab("") +
  labs(fill = "", col = "") +
  scale_fill_manual(values = c("#4472C4","#ED7D31","#92D050")) +
  scale_color_manual(values = "grey") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22))
  

