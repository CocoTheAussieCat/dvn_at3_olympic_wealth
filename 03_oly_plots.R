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

### PLOT 3 M vs F EVENTS -----------------------------------------
# Compare number of events for M and F 
oly_gender <- oly %>%
  group_by(Sex, Games, Year, Event, Season) %>% 
  distinct(Event) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(Sex, Games, Year, Season) %>% 
  summarise(events_gender = sum(count)) %>% 
  glimpse()

gender_labels <- oly_gender %>% 
  ungroup() %>% 
  filter(Season == 'Summer') %>% 
  filter(Year == max(Year)) %>% 
  glimpse()

oly_gender %>% 
  filter(Season == 'Summer') %>% 
  ggplot(aes(x = Year, y = events_gender)) +
  geom_point(aes(colour = Sex)) +
  geom_line(aes(colour = Sex)) +
  geom_text(data = gender_labels, aes(x = Year+2, y = events_gender, 
                                      colour = Sex, label = Sex))+
  oly_theme +
  labs(title = "There are still less Olympic events for females",
       subtitle = 'Number of events, Summer Games only',
       x = "",
       y = "") +
  scale_x_continuous(limits = c(1896, 2020), breaks = seq(1896, 2020, 16)) +
  scale_y_continuous(limits = c(0, 200)) +
  theme(legend.position = "None", panel.grid.major.x = element_blank()) +
  scale_color_manual(values = c(oly_pal[8], oly_pal[1]))

### PLOT 4a & 4b GOLD MEDALS BY COUNTRY ---------------------------------------
# Group by event and games to count gold medals per event, rather than per person
# Eg: Baseball gold medal = 1 rather than 24 (for all team members)
oly_gold <- oly %>%
  filter(Medal == 'Gold') %>% 
  group_by(country, Year, Season, Event, host_games) %>% 
  summarise(gold_medals = n()) %>% 
  mutate(gold_medals = 1) %>% 
  glimpse()

# Summer gold medals per country
oly_gold_summer <- oly_gold %>% 
  filter(Season == 'Summer') %>% 
  group_by(country) %>% 
  summarise(gold_medals = sum(gold_medals)) %>% 
  arrange(desc(gold_medals)) %>% 
  mutate(rank = row_number()) %>% 
  glimpse()

# Winter gold medals per country
oly_gold_winter <- oly_gold %>% 
  filter(Season == 'Winter') %>% 
  group_by(country) %>% 
  summarise(gold_medals = sum(gold_medals)) %>% 
  arrange(desc(gold_medals)) %>% 
  mutate(rank = row_number()) %>% 
  glimpse()

# Join summer to winter 
oly_gold_teams <- oly_gold_summer %>% 
  inner_join(oly_gold_winter, by = "country") %>% 
  rename(gold_summer = gold_medals.x, rank_summer = rank.x,
         gold_winter = gold_medals.y, rank_winter = rank.y) %>% 
  glimpse()

# Summer gold medal bar chart
oly_gold_teams %>% 
  filter(rank_summer <=20) %>% 
  ggplot(aes(x = reorder(country, gold_summer), y = gold_summer)) +
  geom_col(fill = oly_pal[1], width = 0.6) +
  coord_flip() +
  oly_theme +
  labs(title = "United States gold medal haul vastly exceeds any other country's",
       subtitle = 'Summer Games gold medals',
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 100))

oly_gold_teams %>% 
  filter(rank_winter <=20) %>% 
  ggplot(aes(x = reorder(country, gold_winter), y = gold_winter)) +
  geom_col(fill = oly_pal[8], width = 0.6) +
  coord_flip() +
  oly_theme +
  labs(title = "Norway is the leader at Winter Games",
       subtitle = 'Winter Games gold medals',
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20))


### PLOT 5 PEAK GAMES FOR EACH COUNTRY ----------------------------------------
# Find the games where each country won the most gold medals
country_best <- oly_gold %>% 
  filter(Season == 'Summer' & Year >= 1948) %>% 
  group_by(country, Year, Season, host_games) %>% 
  summarise(gold_medals = sum(gold_medals)) %>%
  ungroup() %>% 
  group_by(country) %>% 
  filter(gold_medals == max(gold_medals)) %>% 
  filter(Year == max(Year)) %>% 
  arrange(desc(gold_medals)) %>% 
  mutate(team_year = paste0(country, ": ", Year)) %>% 
  glimpse()

country_label <- country_best %>% 
  filter(country == 'Great Britain') %>% 
  glimpse()

# Bar chart
country_best %>% 
  filter(gold_medals >= 10) %>% 
  ggplot(aes(x = reorder(team_year, gold_medals), y = gold_medals)) +
  geom_col(aes(fill = host_games), width = 0.6) +
  geom_text(data = country_label, aes(x = team_year, y = gold_medals +2, 
                                      label = 'Host', colour = host_games), 
            hjust = 'left') +
  coord_flip() +
  oly_theme +
  labs(title = 'Summer Olympic Games peak gold medal tally, 1948 onwards',
       x = "",
       y = "",
       caption = 'Unified Team in 1992 consisted of 12 of 15 former Soviet republics') +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "None") +
  scale_fill_manual(values = c(oly_pal[4], oly_pal[2])) +
  scale_colour_manual(values = c(oly_pal[2], oly_pal[4])) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  ggsave(here::here('plots', 'peak_gold.png'), height = 6, width = 4 *16/9)

### PLOT 7 - COUNTRIES PER GAME -----------------------------------------------
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

### PLOT 8 - PARTICIPANTS PER 100m --------------------------------------------
men_100m <- oly %>% 
  filter(Event == 'Athletics Men\'s 100 metres') %>% 
  group_by(Year, Season) %>% 
  summarise(participants = n()) %>% 
  glimpse()

men_100m %>% 
  ggplot(aes(x = Year, y = participants, colour = Season)) + 
  geom_line() +
  geom_point() +
  oly_theme +
  scale_color_manual(values = c(oly_pal[1], oly_pal[8])) +
  labs(title = "Men's 100m entrants",
       x = "",
       y = "") +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "None") +
  scale_x_continuous(limits = c(1896, 2024), breaks = seq(1896, 2020, 16))


### PLOT 9 - PARTICIPANTS PER GAME -----------------------------------------------
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

