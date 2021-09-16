############ Script to produce figure change in monetary policy rates (reproduction of REO 2021 Africa - Figure 14)
############ Note: to produce for other regions change sheet filter

# Data into long format -----

long_data <- read_xlsx("../APD_material/raw_data/monetary_policy_rates.xlsx", sheet = "APD") %>% 
  select(country.code,country.name, `1954M1`:ncol(.)) %>% 
  gather("month","rate",`1954M1`:ncol(.)) %>% 
  filter(complete.cases(country.code)) %>% 
  filter(month == "2019M12" | month == "2021M3") %>%
  filter(complete.cases(rate)) %>% 
  merge(income_group, by=c("country.code")) %>% 
  as_tibble()

# Plotting:

long_data %>%
  filter(country.name != "Vietnam" & country.name != "Mongolia") %>% 
  mutate(country.name = fct_reorder(country.name, desc(group))) %>% 
  ggplot(aes(rate,country.name)) +
  geom_line(size = 2, color = "grey") +
  geom_point(data = long_data %>% filter(month == "2021M3"),aes(rate,country.name, col = group), size = 3) +
  labs(col = "") +
  scale_color_manual(values = c("#4472C4","#ED7D31","#92D050")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_blank())

# Exporting intermediate data and plot: -----


# Intermediate data: 

long_data %>%
  filter(country.name != "Vietnam" & country.name != "Mongolia") %>% 
  rio::export("../APD_material/intermediate_data/replication_figures/monetary_rates_figure.xlsx")

# Plot:


ggsave("../APD_material/output/figures/rate_change.pdf",
       height = 7,
       width = 12.5)



  
