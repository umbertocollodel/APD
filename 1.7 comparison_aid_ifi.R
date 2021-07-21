############ Script to produce figure comparison aid across IFIs: 



# Plot: ------


read_xlsx("../APD_material/raw_data/CSIS IFI COVID-19 Response Tracker.xlsx", skip = 2) %>% 
  filter(`Country/Recipient` %in% countrycode(apd_list_countries,"imf","country.name")) %>% 
  filter(!is.na(`Country/Recipient`)) %>% 
  group_by(Institution) %>% 
  summarise_at(vars(contains("Value")),sum, na.rm = T) %>% 
  ungroup() %>%
  mutate(IMF = case_when(Institution == "IMF" ~ "IMF",
                           T ~ "Other IFIs")) %>% 
  mutate(`Institution` = fct_reorder(`Institution`, `Approved Value ($M)`, mean)) %>%
  ggplot(aes(Institution, `Approved Value ($M)`/1000, fill = IMF)) +
  geom_col(width = 0.5, alpha = 0.9) +
  ylab("Amount approved (USD Billions)") +
  xlab("") +
  labs(fill = "") +
  scale_fill_manual(values = c("#4472C4","#ED7D31")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 18)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "none")



# Export:


ggsave("../APD_material/output/figures/amount_aid_apd.pdf",
       height = 6,
       width = 10)
