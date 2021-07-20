############ Script to produce figure change in monetary policy rates (reproduction of REO 2021 Africa - Figure 14): ------


# Data into long format:

long_data <- read_xlsx("../APD_material/raw_data/monetary_policy_rates.xlsx", sheet = "APD") %>% 
  select(country.code,country.name, `1954M1`:ncol(.)) %>% 
  gather("month","rate",`1954M1`:ncol(.)) %>% 
  filter(complete.cases(country.code)) %>% 
  filter(month == "2019M12" | month == "2021M3") %>%
  filter(complete.cases(rate)) 

# Plotting:

long_data %>%
  filter(country.name != "Vietnam" & country.name != "Mongolia") %>% 
  ggplot(aes(rate,country.name)) +
  geom_line(size = 2, color = "grey") +
  geom_point(data = long_data %>% filter(month == "2021M3"),aes(rate,country.name), col = "#4472C4", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_blank())

# Exporting:

ggsave("../APD_material/output/figures/rate_change.pdf",
       height = 7,
       width = 12.5)



  
