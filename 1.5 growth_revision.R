########## Script to produce figure revision of GDP growth:


readRDS("~/Dropbox/Forecasts_Time_Covid/Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS") %>% 
  filter(country_code %in% apd_list_countries) %>% 
  filter(horizon == "Jan") %>% 
  mutate(diff = -(Actual - abs(value))) %>% 
  mutate(country = fct_reorder(country, diff, mean)) %>% 
  ggplot(aes(diff, country)) +
  geom_col(width = 0.5) +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 20))

# Export:

ggsave("../APD_material/output/figures/growth_revision.pdf")

