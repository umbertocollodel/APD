########### Script to produce figure on deaths by country 


# Calculate cumulative deaths by country and take different snapshots over time: ------
# Note: when no data on cases on a month, set equal to 0

deaths_df <- read_xlsx("~/Desktop/ERP matrix.xlsx", sheet = "OWID Data") %>%
  filter(imf %in% apd_list_countries) %>% 
  split(.$iso3c) %>% 
  map(~ .x %>% mutate(NewConfirmedDeaths = case_when(is.na(NewConfirmedDeaths)~ 0,
                                                     T ~ NewConfirmedDeaths))) %>% 
  map(~ .x %>% mutate(cumsum_deaths = cumsum(NewConfirmedDeaths))) %>% 
  map(~ .x %>% select(iso3c, AlternativeDate, cumsum_deaths)) %>%
  bind_rows()


# Take different snapshots in time (April, June, August)

time_chunks=c("2020M4","2020M6","2020M8")

df <- time_chunks %>% 
  map(~ deaths_df %>% filter(AlternativeDate == .x))

# Df with geographic coordinates: -----

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  rename(iso3c = iso_a3)


# Plot: ------

df %>% 
  map(~ .x %>% merge(world, by=c("iso3c"))) %>% 
  map(~ .x %>% 
  ggplot(aes(geometry = geometry, fill = cumsum_deaths)) +
  geom_sf() +
  scale_fill_gradient(low = "white", high = "red") +
  xlim(60,190) +
  theme_minimal() +
  theme(legend.position = "none"))

# Export:




  