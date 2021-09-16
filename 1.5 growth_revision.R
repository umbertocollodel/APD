########## Script to produce table revision of GDP growth and figure with pre-2019 developments in real GDP:
# Note: not standardized for other regions


# Table GDP revision after crisis: -----

readRDS("~/Dropbox/Forecasts_Time_Covid/Forecasts_Time_Covid_material/intermediate_data/weo_2020.RDS") %>% 
  filter(country_code %in% apd_list_countries) %>% 
  filter(horizon == "Jan") %>% 
  mutate(diff = -(Actual - abs(value))) %>% 
  select(country, value, Actual,diff) %>% 
  arrange(-diff) %>% 
  setNames(c("Country","January 2020 Projection","April 2021 Estimate","Difference from Pre-Pandemic Jan. 2020
  World Economic Outlook (abs)")) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  write.table(file = "../APD_material/output/tables/growth_revision.txt",
              row.names = F,
              quote = F,
              sep = ",")


# Figure GDP growth leading to the crisis: -----

read_xlsx("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/weo_rgdp.xlsx", sheet = "apr2021") %>% 
  filter(Country == "Asia and Pacific (EXR) WEO" | Country == "Advanced Asia" | Country == "Emerging and Developing Asia") %>% 
  gather("year","value",`1950`:ncol(.)) %>% 
  group_by(Country) %>% 
  mutate(growth = (value - dplyr::lag(value,1))/dplyr::lag(value,1)*100) %>% 
  filter(year >= "2010" & year <= "2019") %>% 
  ungroup() %>% 
  mutate(Country = case_when(Country == "Asia and Pacific (EXR) WEO" ~ "Asia and Pacific",
                             T ~ Country)) %>% 
  mutate(Country = factor(Country,levels = c("Asia and Pacific","Advanced Asia","Emerging and Developing Asia"))) %>% 
  ggplot(aes(year,growth, group = Country, col = Country)) +
  geom_line(size = 1.5) +
  xlab("") +
  ylab("") +
  labs(col="") +
  scale_x_discrete(breaks = c("2010","2013","2016","2019")) +
  scale_color_manual(values = c("#4472C4","#ED7D31","#92D050")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18))


# Export:

ggsave("../APD_material/output/figures/growth_deceleration_precrisis.pdf",
       height = 7,
       width = 12.5)
