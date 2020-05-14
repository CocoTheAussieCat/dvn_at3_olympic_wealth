### CREATES FULL DATASET AS TIDY RDS FILE -------------------------------------
# Olympic data from Kaggle
# Lat-long coordinates from World Map Harvard
# GDP per capita data from Gapminder
# Joined into single tidy dataframe called oly.rds

source("00_packages.R")

if (!exists("lat_long_org")) {
  lat_long_org <- readRDS(here("olympic_data", "lat_long_org.rds"))
}

if (!exists("country_org")) {
  country_org <- readRDS(here("olympic_data", "country_org.rds"))
}

if (!exists("oly_org")) {
  oly_org <- readRDS(here("olympic_data", "oly_org.rds"))
}

if (!exists("noc_to_country")) {
  noc_to_country <- readRDS(here("olympic_data", "noc_to_country.rds"))
}

if (!exists("gdp_clean")) {
  gdp_clean <- readRDS(here("olympic_data", "gdp_clean.rds"))
}

# Set data types, remove numbers from Team names
# Joined noc_to_country to get country names
oly_simple <- oly_org %>% 
  mutate(Sex = as_factor(Sex),
         Season = as_factor(Season),
         Sport = as_factor(Sport),
         Event = as_factor(Event)) %>% 
  mutate(Medal = fct_explicit_na(Medal, "None"),
         Team = str_replace(Team, "-\\d", " "),
         Team = str_squish(Team)) %>% 
  left_join(noc_to_country, by = 'NOC') %>% 
  glimpse()

# File with host country AND host city (manually created)
host_cities <- read_csv(here::here('olympic_data', 'host_city.csv')) %>% 
  rename(Games = Games, City = City, host_country = Country) %>% 
  select(Games, host_country)

# Join host country and create flag
oly_host <- oly_simple %>%
  left_join(host_cities, by = 'Games') %>% 
  mutate(host_games = ifelse(country == host_country, T, F))

# Join lat_long data
lat_long_summary <- lat_long_org %>% 
  select(geounit, Latitude, Longitude) %>% 
  write_rds(here::here('tidy', 'lat_long_summary.rds'))

# Dataframe to use for analysis
oly <- oly_host %>% 
  left_join(lat_long_summary, by = c('country' = 'geounit')) %>% 
  left_join(gdp_clean, by = c('country' = 'country', 'Year' = 'year')) %>%
  write_rds(here::here('tidy', 'oly.rds'))



  

