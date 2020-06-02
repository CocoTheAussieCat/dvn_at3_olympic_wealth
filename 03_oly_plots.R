source("00_packages.R")

if (!exists("oly")) {
  oly <- readRDS(here("tidy", "oly.rds"))
}

if (!exists("lat_long_summary")) {
  lat_long_summary <- readRDS(here("tidy", "lat_long_summary.rds"))
}

### PLOT THEME ----------------------------------------------------------------
oly_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, face = 'italic', hjust = 0))

# Colour palette
oly_pal <- c('#a50026','#d73027','#f46d43','#fdae61',
             '#fee090','#e0f3f8','#abd9e9','#74add1',
             '#4575b4','#313695')

### PLOT 1 NUMBER OF SPORTS OVER TIME -----------------------------------------
# Count number of sports per games
oly_sport <- oly %>%
  #filter(Year >=1948) %>% 
  group_by(Games, Year, Sport, Season) %>% 
  distinct(Sport) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(Games, Year, Season) %>% 
  summarise(total_sports = sum(count)) %>% 
  glimpse()

oly_sport_labels <- oly_sport %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  filter(Year == max(Year)) %>% 
  glimpse()

oly_sport %>% 
  ggplot(aes(x = Year, y = total_sports)) +
  geom_point(aes(colour = Season)) +
  geom_line(aes(colour = Season)) +
  geom_text(data = oly_sport_labels, aes(x = Year+2, y = total_sports, label = Season, colour = Season, hjust = 'left')) +
  oly_theme +
  scale_fill_manual(values = c(oly_pal[3], oly_pal[9])) +
  scale_colour_manual(values = c(oly_pal[3], oly_pal[9])) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) +
  scale_x_continuous(limits = c(1896, 2024), breaks = seq(1896, 2016, 16)) +
  theme(legend.position = "None", panel.grid.major.x = element_blank()) +
  labs(title = 'Number of sports',
       x = "",
       y = "") +
  ggsave(here::here('plots', 'sports.png'), height = 4, width = 4 *16/9)

### PLOT 2 NUMBER OF EVENTS OVER TIME -----------------------------------------
# Count number of events per games
oly_events <- oly %>%
  group_by(Games, Year, Event, Season) %>% 
  distinct(Event) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(Games, Year, Season) %>% 
  summarise(total_events = sum(count)) %>% 
  glimpse()

oly_event_labels <- oly_events %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  filter(Year == max(Year)) %>% 
  glimpse()

oly_events %>% 
  ggplot(aes(x = Year, y = total_events)) +
  geom_point(aes(colour = Season)) +
  geom_line(aes(colour = Season)) +
  geom_text(data = oly_event_labels, aes(x = Year + 2, y = total_events, label = Season, colour = Season), hjust = 'left') +
  oly_theme +
  scale_fill_manual(values = c(oly_pal[3], oly_pal[9])) +
  scale_colour_manual(values = c(oly_pal[3], oly_pal[9])) +
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, 50)) +
  scale_x_continuous(limits = c(1896, 2024), breaks = seq(1896, 2016, 16)) +
  theme(legend.position = "None", panel.grid.major.x = element_blank()) +
  labs(title = 'Number of events',
       x = "",
       y = "") +
  ggsave(here::here('plots', 'events.png'), height = 4, width = 4 *16/9)

### PLOT 3 - COUNTRIES PER GAME -----------------------------------------------
# Number of participating countries
total_countries <- oly %>%  
  group_by(Year, Season, NOC) %>% 
  summarise() %>% 
  ungroup() %>% 
  group_by(Year, Season) %>% 
  summarise(countries = n())

total_countries_labels <- total_countries %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  filter(Year == max(Year))

total_countries %>%
  ggplot(aes(x = Year, y = countries)) +
  geom_point(aes(colour = Season)) +
  geom_segment(aes(xend = Year, yend = 0, colour = Season), alpha = 0.5) +
  geom_text(data = total_countries_labels, aes(x = Year-10, y = countries+20, 
                                               colour = Season, label = Season), 
            hjust = 'right') +
  facet_wrap(~Season, nrow = 2, scales = "free_x") +
  oly_theme +
  scale_color_manual(values = c(oly_pal[3], oly_pal[10])) +
  labs(title = 'Countries participating at each Games',
       x = "",
       y = "") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "None",
        strip.text = element_blank()) +
  scale_x_continuous(limits = c(1896, 2024), breaks = seq(1896, 2020, 16)) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50)) +
  ggsave(here::here('plots', 'total_countries.png'), height = 4, width = 4 *16/9)

### PLOT 4 - PARTICIPANTS PER GAME -----------------------------------------------
# Number of participants, adjusting for where a competitor takes part in more
# than 1 event, so has more than 1 observation per games
total_part <- oly %>%  
  group_by(Year, Season, ID) %>% 
  summarise(total = 1) %>% 
  ungroup() %>% 
  group_by(Year, Season) %>% 
  summarise(total = sum(total)/10^3) %>% 
  glimpse()

total_part_labels <- total_part %>% 
  ungroup() %>% 
  group_by(Season) %>% 
  filter(Year == max(Year))

total_part %>%
  ggplot(aes(x = Year, y = total)) +
  geom_point(aes(colour = Season)) +
  geom_segment(aes(xend = Year, yend = 0, colour = Season), alpha = 0.5) +
  geom_text(data = total_part_labels, aes(x = Year, y = total + 2, 
                                               colour = Season, label = Season), 
            hjust = 'left') +
  facet_wrap(~Season, nrow = 2, scales = "free") +
  oly_theme +
  scale_color_manual(values = c(oly_pal[3], oly_pal[10])) +
  labs(title = 'Athletes participating at each Games (thousands)',
       x = "",
       y = "") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "None",
        strip.text = element_blank()) +
  scale_x_continuous(limits = c(1896, 2024), breaks = seq(1896, 2020, 16)) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 12, 2), 
                     labels = number_format(accuracy = 1, suffix = 'k')) +
  ggsave(here::here('plots', 'total_part.png'), height = 4, width = 4 *16/9)

### DATAFRAME FOR MAP IN STREAMLIT --------------------------------------------
# Country, gold medals, year, latitude, longitude
gold_time_series <- oly_gold %>% 
  ungroup() %>% 
  select(country, gold_medals, Year) %>% 
  left_join(lat_long_summary, by = c('country' = 'geounit')) %>% 
  group_by(country, Year, Latitude, Longitude) %>% 
  summarise(gold_medals = sum(gold_medals)) %>% 
  filter(!is.na(Latitude) & !is.na(Longitude)) %>% 
  write_csv(here::here('python', 'gold_time_series.csv'))

### SUMMARY CALCULATIONS FOR REPORT -------------------------------------------

# Host city count
host_cities <- oly %>% group_by(Year, Season, City) %>% 
  summarise()

# Total medals
total_medals <- gold_time_series %>% 
  group_by(Year) %>% 
  summarise(total = sum(gold_medals)) 

sum(total_medals$total) * 3

oly %>%  
  group_by(Year, Season, NOC) %>% 
  summarise() %>% 
  ungroup() %>% 
  group_by(Year, Season) %>% 
  summarise(countries = n())

### PARTICIPANTS BY COUNTRY ---------------------------------------------------
# Number of participants by country for each games
# exported to use in python map plots
country_part <- oly %>%  
  group_by(Year, Season, country, Latitude, Longitude, iso_a3 ,ID) %>% 
  summarise(total = 1) %>% 
  ungroup() %>% 
  group_by(Year, Season, country, Latitude, Longitude, iso_a3) %>% 
  summarise(total = sum(total)) %>% 
  write_csv(here::here("python", "country_part.csv"))



