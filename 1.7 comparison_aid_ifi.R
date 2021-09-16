############ Script to produce figure comparison aid across IFIs: 
# Note: for other regions change filter list countries


# Prepare dataframe: -----

df <- read_xlsx("../APD_material/raw_data/CSIS IFI COVID-19 Response Tracker.xlsx", skip = 2) %>% 
  filter(`Country/Recipient` %in% countrycode(apd_list_countries,"imf","country.name")) %>% 
  filter(!is.na(`Country/Recipient`)) %>%
  mutate(Institution = case_when(Institution == "AsDb" ~ "AsDB",
                                 T ~ Institution)) %>% 
  group_by(Institution)


# Calculate total amount arrangements different institution: ------

df_amount <- df %>% 
  summarise_at(vars(contains("Value")),sum, na.rm = T) %>% 
  ungroup() %>%
  mutate(IMF = case_when(Institution == "IMF" ~ "IMF",
                           T ~ "Other IFIs")) %>% 
  mutate(`Institution` = fct_reorder(`Institution`, `Approved Value ($M)`, mean)) 

# Plot amount arrangements different institution: ------

df_amount %>%
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

# Export intermediate data:

df_amount %>% 
  rio::export("../APD_material/intermediate_data/replication_figures/amount_aid_figure.xlsx")


# Export plot:

ggsave("../APD_material/output/figures/amount_aid_apd.pdf",
       height = 6,
       width = 10)


# Calculate number of countries with an arrangement: ----

df_number <- df %>% 
  distinct(`Country/Recipient`) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(IMF = case_when(Institution == "IMF" ~ "IMF",
                         T ~ "Other IFIs")) %>% 
  mutate(Institution = fct_reorder(Institution, n, mean)) 

# Plot number of countries with an arrangement: ----

df_number %>% 
  ggplot(aes(Institution, n, fill = IMF)) +
  geom_col(width = 0.5, alpha = 0.9) +
  scale_fill_manual(values = c("#4472C4","#ED7D31")) +
  xlab("") +
  ylab("Number of Countries") +
  theme_minimal() +
  theme(axis.title = element_text(size = 22),
          axis.text = element_text(size = 18)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "none")

# Export intermediate data:

df_number %>% 
  rio::export("../APD_material/intermediate_data/replication_figures/number_aid_figure.xlsx")


# Export plot:

ggsave("../APD_material/output/figures/ncountries_aid_apd.pdf",
       height = 6,
       width = 10)
  