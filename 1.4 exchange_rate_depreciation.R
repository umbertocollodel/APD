########## Script to produce figure exchange rate depreciation:


months=c("2008-08","2008-11","2009-12","2020-01","2020-04","2021-04")

read.xlsx("../APD_material/raw_data/financial_nominal_exchange_rate.xlsx", sheet = "APD") %>% 
  as_tibble() %>% 
  gather("month","er",`1980-01`:ncol(.)) %>% 
  filter(month %in% months) %>% 
  mutate(indicator = case_when(str_detect(month,"8|9") ~ "GFC",
                               T ~ "Covid")) %>% 
  split(.$indicator) %>% 
  map(~ .x %>% spread(month,er)) %>%
  map(~ .x %>% setNames(c("country.code","country.name","Indicator","indicator","first_period","second_period","third_period"))) %>%
  map(~ .x %>% mutate(depreciation_initial = ((second_period - first_period)/first_period * 100),
                      depreciation_final = ((third_period - first_period)/first_period*100))) %>% 
  map(~ .x %>% select(contains("country"), contains("depreciation"))) %>% 
  bind_rows(.id = "period") %>%
  filter(country.name %in% c("Australia","Bangladesh","Indonesia","Japan","Thailand","Nepal","Korea",
         "New Zealand","Vietnam", "Malaysia","Papua New Guinea","Tonga", "Philippines", "Singapore", "India",
         "Sri Lanka","China")) %>% 
  ggplot(aes(country.name)) +
  geom_col(data = . %>% filter(period == "Covid"),aes(y=depreciation_initial, fill = "Covid-19 (Jan-Apr 2020)")) +
  geom_point(data = . %>% filter(period == "GFC"),aes(y=depreciation_initial, col = "GFC (Aug-Nov 2008)"), size = 3) +
  ylab("Percent change of exchange rate \n (+ depreciation of local currency \nvis-a-vis US$)") +
  xlab("") +
  labs(col = "",fill = "") +
  scale_fill_manual(values = "gray") +
  scale_color_manual(values = "#4472C4") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20))

# Export:

ggsave("../APD_material/output/figures/exchange_rate_depreciation.pdf",
       height = 7,
       width = 12.5)
  
