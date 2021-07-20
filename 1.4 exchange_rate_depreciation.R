########## Script to produce figure exchange rate depreciation:


months=c("2008-08","2009-02","2020-01","2020-06")

read.xlsx("../APD_material/raw_data/financial_nominal_exchange_rate.xlsx", sheet = "APD") %>% 
  as_tibble() %>% 
  gather("month","er",`1980-01`:ncol(.)) %>% 
  filter(month %in% months) %>% 
  mutate(indicator = case_when(str_detect(month,"8|9") ~ "GFC",
                               T ~ "Covid")) %>% 
  split(.$indicator) %>% 
  map(~ .x %>% spread(month,er)) %>%
  map(~ .x %>% setNames(c("country.code","country.name","Indicator","indicator","first_period","second_period"))) %>%
  map(~ .x %>% mutate(depreciation = ((second_period - first_period)/first_period * 100))) %>% 
  map(~ .x %>% select(contains("country"), contains("depreciation"))) %>% 
  map(~ .x %>% setNames(c("country.code","country.name","depreciation"))) %>% 
  bind_rows(.id = "period") %>% 
  ggplot(aes(country.name, depreciation)) +
  geom_col(data = . %>% filter(period == "Covid"),aes(y=depreciation), fill = "gray") +
  geom_point(data = . %>% filter(period == "GFC"),aes(y=depreciation), col = "#4472C4", size = 3) +
  ylab("Percent change of exchange rate \n (+ depreciation of local currency vis-a-vis US$") +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20))


ggsave("../APD_material/output/figures/exchange_rate_depreciation.pdf")
  
