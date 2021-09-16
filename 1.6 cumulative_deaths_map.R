########### Script to produce figure on deaths by country (map) over time
# Note: for other regions change filter list of countries


# Calculate cumulative deaths by country and take different snapshots over time: ------
# Note: when no data on cases on a month, set equal to 0

deaths_df <- read_xlsx("../APD_material/raw_data/ERP matrix.xlsx", sheet = "OWID Data") %>%
  filter(imf %in% apd_list_countries) %>% 
  split(.$iso3c) %>% 
  map(~ .x %>% mutate(NewConfirmedDeaths = case_when(is.na(NewConfirmedDeaths)~ 0,
                                                     T ~ NewConfirmedDeaths))) %>% 
  map(~ .x %>% mutate(cumsum_deaths = cumsum(NewConfirmedDeaths))) %>% 
  map(~ .x %>% select(iso3c, AlternativeDate, cumsum_deaths)) %>%
  bind_rows()


# Take different snapshots in time (April, June, August)

time_chunks=c("2020M4","2020M10")

df <- time_chunks %>% 
  map(~ deaths_df %>% filter(AlternativeDate == .x))

# Merge two snapshots dfs with geographic coordinates: -----

# Create df with geographic coordinates:

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(iso3c = iso_a3)

# Merge with the two snapshots:

map_df <- df %>% 
  map(~ .x %>% merge(world, by=c("iso3c")))


# Plot and export: ------

maps_deaths <- map_df %>% 
  map(~ .x %>% 
  ggplot(aes(geometry = geometry, fill = cumsum_deaths)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "red") +
  xlim(60,190) +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=14)) +
  theme(axis.text = element_blank()))

# Export intermediate data:
# Note: problem exporting this df

map_df %>% 
  map2(time_chunks, ~ rio::export(.x,
                                  paste0("../APD_material/intermediate_data/replication_figures/map_figure_",.y,".xlsx")))

# Export plot:

maps_deaths %>% 
  walk2(time_chunks, ~ ggsave(paste0("../APD_material/output/figures/cumsum_deaths_map",.y,".pdf"),.x))




  