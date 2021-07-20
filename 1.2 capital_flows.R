# Script to produce figure capital flows in the region: ------


read_xlsx("../APD_material/raw_data/financial_capital_flows.xlsx", sheet = "APD") %>% 
  gather("quarter","value", `2005-Q1`:ncol(.)) %>% 
  group_by(quarter, `Type Inflow`) %>% 
  summarise(sum = sum(as.numeric(value), na.rm = T)) %>% 
  ggplot(aes(quarter, sum, fill = `Type Inflow`)) +
  geom_col(position = "stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
