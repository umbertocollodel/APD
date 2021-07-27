# Income group: -----

income_group <- read_xlsx("~/Dropbox/When_where_and_why/When_where_and_why_material/raw_data/country_group.xlsx") %>% 
  mutate(group = case_when(adv == 1 ~ "Advanced",
                           eme == 1 & lidc == 0 ~ "EM",
                           eme == 1 & lidc == 1 ~ "LIDC"
  )) %>% 
  select(ifscode, group) %>% 
  rename(country.code = ifscode)





# Cleaning fiscal response dataframe: ------

fiscal_response_df <- read_xlsx("../APD_material/raw_data/fiscal-measures-covid_19.xlsx", skip = 4, sheet = 2) %>% 
  select(`...1`,`Additional spending or foregone revenues...14`,`...15`,`...16`) %>% 
  slice(3:nrow(.)) %>% 
  filter(complete.cases(.)) %>% 
  setNames(c("country.name","total","health","non_health")) %>% 
  mutate(country.code = countrycode(country.name,"country.name","imf")) %>% 
  filter(complete.cases(country.code)) %>% 
  filter(country.code %in% apd_list_countries) %>% 
  mutate_at(vars(total,contains("health")),funs(as.numeric(.)))
  
  

# Merge and plot: ----- 

fiscal_response_df %>%     
  merge(income_group) %>% 
  as_tibble() %>% 
  gather("type","value",total:non_health) %>% 
  group_by(group, type) %>% 
  summarise(average_response = mean(value)) %>% 
  filter(type != "total") %>% 
  mutate(type = case_when(type == "health" ~ "Health sector",
                          T ~ "Non-health sector")) %>% 
  ggplot(aes(group,average_response, fill = type)) +
  geom_col(alpha = 0.9,position = "stack") +
  theme_minimal() +
  ylab("% of GDP") +
  xlab("") +
  labs(fill = "") +
  scale_fill_manual(values = c("#4472C4","#ED7D31")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 22))


# Export:

ggsave("../APD_material/output/figures/fiscal_response_gdp.pdf",
       height = 7,
       width = 12.5)


# Merge and plot top 3 by income group: -----

fiscal_response_df %>%     
  merge(income_group) %>% 
  as_tibble() %>% 
  group_by(group) %>% 
  arrange(-total) %>% 
  slice(1:3) %>% 
  mutate(country.name = fct_reorder(country.name,total, mean, .desc = T)) %>% 
  ggplot(aes(total,country.name, fill = group)) +
  geom_col(width = 0.6) +
  labs(fill = "") +
  scale_y_discrete(limits=rev) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  scale_fill_manual(values = c("#4472C4","#ED7D31","#92D050")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_blank())
  
    

  
