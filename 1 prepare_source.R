# Prepare the environment: -----


remove(list = ls())


packages=c("tidyverse","openxlsx","readxl",
           "countrycode","rnaturalearth","rnaturalearthdata","sf")


lapply(packages, function(x){
  do.call("require", list(x))
}
)





# List of APD countries: -----

apd_list_countries <- read.xlsx("../APD_material/raw_data/countryinformation.xlsx", sheet = "Country data") %>% 
  select(2, AFR_IMF, APD_IMF, EUR_IMF, MCD_IMF, WHD_IMF) %>% 
  filter(complete.cases(`CODE_IMF`)) %>% 
  gather("region","value",AFR_IMF:WHD_IMF) %>% 
  filter(complete.cases(value)) %>% 
  mutate(region = str_extract(region, "^.{3}")) %>%
  filter(region == "APD") %>% 
  .$`CODE_IMF`

# Income group classification: -----

income_group <- read_xlsx("../APD_material/raw_data/country_group.xlsx") %>% 
  rename(country.code = ifscode) %>%
  mutate(group = case_when(adv == 1 ~ "Advanced",
                           eme == 1 & lidc == 0 ~ "EM",
                           eme == 1 & lidc == 1 ~ "LIDC")) %>% 
  select(country.code, group)

