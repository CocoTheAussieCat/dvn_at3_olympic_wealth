source("00_packages.R")

### IMPORT DATA AND WRITE TO RDS FILES ----------------------------------------
# 1. Olympic data sourced from Kaggle
oly_org <- read_csv(here::here("olympic_data", "athlete_events.csv")) %>% 
  write_rds(here::here("olympic_data", "oly_org.rds"))

country_org <- read_csv(here::here("olympic_data", "noc_regions.csv")) %>% 
  write_rds(here::here("olympic_data", "country_org.rds"))

# 2. Country lat and long for maps sourced from worldmap.harvard.edu
country_url <- "http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0"

lat_long_org <- read_csv(here::here('olympic_data', 'lat_long_org.csv')) %>% 
  write_rds(here::here("olympic_data", "lat_long_org.rds"))

# Manually cleaned up file that matches NOC to country name
# Deals with countries that have changed names and
# weird names from pre-WW1
noc_to_country <- read_csv(here::here('olympic_data', 'noc_to_country.csv')) %>% 
  group_by(NOC, Team) %>% 
  summarise() %>% 
  rename(country = Team) %>% 
  write_rds(here::here('olympic_data', 'noc_to_country.rds'))

# 3. GDP data from gapminder
# Pivot longer to three columns = country, year, gdp
gdp_org <- read_csv(here::here('olympic_data', 'gdp.csv')) %>% 
  pivot_longer(cols = c(`1960`:`2018`), names_to = 'year', values_to = 'gdp')

# Read in manually created names list to clean up names
names_to_fix <- read_csv(here::here('olympic_data', 'gdp_name_cleaning.csv')) %>% 
  glimpse()

# Join names_to_fix to gdp and replace names where required
gdp_clean <- gdp_org %>% 
  left_join(names_to_fix, by = 'country') %>% 
  mutate(country = ifelse(!is.na(oly_country), oly_country, country),
         year = as.numeric(year)) %>% 
  select(-oly_country) %>% 
  write_rds(here::here('olympic_data', 'gdp_clean.rds'))

# 4. Soviet Union GDP from UNDataExplorer
ussr_org <- read_csv(here::here('olympic_data', 'soviet_gdp.csv')) %>% 
  glimpse()



