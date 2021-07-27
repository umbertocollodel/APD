####### Script to produce capital flows figure 



# Basic cleaning flows and reserves data: ------
# Note: removed 2021-Q1 because data not available for all countries

paths=c("~/Dropbox/APD/APD_material/raw_data/financial_capital_flows.xlsx",
        "~/Dropbox/APD/APD_material/raw_data/financial_reserves.xlsx")



list_flows <- paths %>% 
  map(~ read_xlsx(.x, sheet = "APD")) %>% 
  map(~ .x %>% mutate(country.code = as.character(country.code))) %>% 
  map(~ .x %>% filter(country.code %in% apd_list_countries)) %>%
  map(~ .x %>% select(-`2021-Q1`)) %>% 
  map(~ .x %>% gather("quarter","value",`2005-Q1`:`2020-Q4`)) %>% 
  map(~ .x %>% mutate(value = as.numeric(value)))


# Change reserves data from stock to flow: ------


list_flows[[2]] <- list_flows[[2]] %>% 
  group_by(country.code) %>% 
  mutate(value_flow = -(value - dplyr::lag(value,1))) %>% 
  mutate(`Type Inflow` = "International Reserves") %>%
  select(-value) %>% 
  select(country, country.code, `Type Inflow`,everything()) %>% 
  setNames(c("country","country.code","Type Inflow", "Indicator","Unit","quarter","value")) %>% 
  ungroup()


# Plot evolution over time: ------

list_flows[[1]] %>%
  filter(country != "Japan") %>%
  filter(quarter >= "2014-Q1") %>% 
  group_by(quarter, `Type Inflow`) %>% 
  summarise(sum = sum(value, na.rm = T)/1000) %>% 
  group_by(quarter) %>% 
  mutate(total = sum(sum)) %>% 
  ggplot(aes(`quarter`,sum, fill = `Type Inflow`)) +
  geom_col(width = 0.4) +
  geom_line(aes(y=total, group = 1, col = "Total"), size = 1.5) +
  xlab("") +
  ylab("USD Billions") +
  labs(fill = "", col = "") +
  scale_fill_manual(values = c("#4472C4","#ED7D31","#92D050","grey")) +
  theme_minimal() +
  scale_color_manual(values = "black") +
  scale_x_discrete(breaks = map_chr(2014:2020, ~ paste0(.x,"-Q1"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22))


# Export:

ggsave("../APD_material/output/figures/gross_capital_flows.pdf",
       height = 7,
       width = 12.5)

  
  