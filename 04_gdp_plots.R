source("00_packages.R")
library(treemapify)

if (!exists("oly")) {
  oly <- readRDS(here("tidy", "oly.rds"))
}

if (!exists("lat_long_summary")) {
  lat_long_summary <- readRDS(here("tidy", "oly.rds"))
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

### GDP per capita for events in 2016
event_gdp <- oly %>% 
  filter(Year == 2016) %>% 
  drop_na() %>% 
  group_by(Event) %>% 
  summarise(gdp_event = sum(gdp)/n()) %>% 
  arrange(desc(gdp_event)) %>% 
  glimpse()

### GDP per capita by sport in 2016 -----------------------------------
sport_gdp <- oly %>% 
  filter(Year == 2016) %>% 
  drop_na() %>% 
  group_by(Sport) %>% 
  summarise(gdp_sport = sum(gdp)/n()) %>% 
  arrange(desc(gdp_sport)) %>% 
  mutate(rank = row_number(),
         position = case_when(gdp_sport/10^3 >= 30 ~'Highest',
                              gdp_sport/10^3 >= 25 ~'Middle',
                              gdp_sport/10^3 < 25~'Lowest')) %>% 
  glimpse()

### PLOT 1 HIGHEST AND LOWEST GDP PER CAPITA SPORTS ---------------------------
sport_gdp %>% 
  ggplot(aes(x = reorder(Sport, gdp_sport), y = gdp_sport/10^3)) +
  geom_point(aes(colour = position)) +
  geom_segment(aes(xend = Sport, yend = 0, colour = position)) +
  coord_flip() +
  oly_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "None",
        strip.text = element_blank(),
        panel.grid.major = element_blank()) +
  labs(title = 'Mean GDP per capita of countries by sport, 2016 Summer Games',
       caption = 'GDP per capita in thousands of US dollars, 2010 nominal terms \nGDP per capita weighted by number of participants from each country',
       x = '',
       y = '') +
  scale_color_manual(values = c(oly_pal[9], oly_pal [2], oly_pal[4])) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), 
                     labels = dollar_format(suffix = "k")) +
  ggsave(here::here('plots', 'gdp_sport.png'), height = 6, width = 4 *16/9)


equest <- oly %>% 
  filter(Year==2016 & Sport== 'Equestrianism') %>% 
  group_by(Team) %>% 
  summarise(gdp = sum(gdp)/n(), count = n()) %>% 
  arrange(desc(count)) %>% 
  glimpse()

weight <- oly %>% 
  filter(Year==2016 & Sport== 'Weightlifting') %>% 
  group_by(Team) %>% 
  summarise(gdp = sum(gdp)/n(), count = n()) %>%  
  arrange(desc(count)) %>% 
  glimpse()

equest %>% 
  ggplot(aes(x = reorder(Team, count), y = count)) +
  geom_col(aes(fill = gdp)) +
  scale_fill_distiller(palette = 'RdYlBu') +
  coord_flip() +
  oly_theme

equest %>% 
  ggplot(aes(area = count, fill = gdp/10^3, subgroup = Team)) +
  geom_treemap() +
  scale_fill_distiller(palette = 'RdYlBu', direction = 1) +
  geom_treemap_subgroup_border(colour = 'white')+
  #subgroup heading in white
  geom_treemap_subgroup_text(color="black", size = 15, place = 'center')+
  #all other group text in black
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Countries competing in Equestrian at 2016 Summer Games',
       subtitle = 'Size of box represents number of competitors',
       fill = "GDP per capita (US$ thousand)") +
  oly_theme +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggsave(here::here('plots', 'equest_2016.png'), height = 4, width = 4 *16/9)

weight %>% 
  ggplot(aes(area = count, fill = gdp/10^3, subgroup = Team)) +
  geom_treemap() +
  scale_fill_distiller(palette = 'RdYlBu', direction = 1) +
  geom_treemap_subgroup_border(colour = 'white')+
  #subgroup heading in white
  geom_treemap_subgroup_text(color="black", size = 15, place = 'center')+
  #all other group text in black
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Countries competing in Weightlifting at 2016 Summer Games',
       subtitle = 'Size of box represents number of competitors',
       fill = "GDP per capita (US$ thousand)") +
  oly_theme +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggsave(here::here('plots', 'weight_2016.png'), height = 4, width = 4 *16/9)

### PLOT 2: SUMMER VS WINTER GDP PER CAP --------------------------------------
season_gdp <- oly %>% 
  filter(Year >= 1994) %>% 
  drop_na() %>% 
  group_by(Year, Season, City) %>% 
  summarise(gdp = mean(gdp)) %>% 
  group_by(Season) %>% 
  mutate(games_no = row_number()) %>% 
  glimpse()

# Create labels for chart and nudge position so don't overlap points
season_gdp_labels <- season_gdp %>% 
  filter(Year == 2004 | Year == 2006) %>% 
  mutate(gdp = ifelse(Year == 2004, gdp/10^3 - 3, gdp/10^3 + 3)) %>% 
  glimpse()

season_gdp_labels <- season_gdp %>% 
  mutate(gdp = ifelse(Season == "Summer", gdp/10^3 - 3, gdp/10^3 + 3)) %>% 
  glimpse()

season_gdp %>% 
  ggplot(aes(x = Year, y = gdp/10^3)) +
  geom_line(aes(group = games_no), colour = 'grey') +
  geom_point(aes(colour = Season), size = 4) +
  geom_text(data = season_gdp_labels, aes(x = Year, y = gdp, 
                                          label = paste0(Year, " ", City), 
                                          colour = Season), size = 3) +
  oly_theme +
  scale_x_continuous(limits = c(1992, 2020), breaks = seq(1992, 2016, 4)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5), 
                     labels = dollar_format(suffix = "k")) +
  labs(title = "Mean GDP per capita of countries competing at Olympic Games",
       caption = "GDP per capita (US$ thousand, 2010 inflation-adjusted) \nGDP weighted by number of participants per country",
       x = "",
       y = "") +
  scale_colour_manual(values = c(oly_pal[3], oly_pal[9])) +
  theme(legend.position = c(0.1,0.1), legend.direction = "horizontal", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank()) +
  ggsave(here::here('plots', 'season_gdp.png'), height = 4, width = 4 *16/9)
  

### PLOT 3: COUNTRIES & GDP AT SUMMER VS WINTER -------------------------------
season_gdp_detailed <- oly %>% 
  filter(Year >= 1994) %>% 
  drop_na() %>% 
  glimpse()

season_gdp_summary <- oly %>%
  filter(Year >= 1994 & gdp < 100*10^3) %>% 
  drop_na() %>% 
  group_by(Season, Team) %>% 
  summarise(gdp = sum(gdp)/n(), count = n()) %>% 
  arrange(desc(count)) %>% 
  glimpse()
  

# Winter country tree plot
season_gdp_summary %>% 
  filter(Season == 'Winter') %>% 
  ggplot(aes(area = count, fill = gdp/10^3, subgroup = Team)) +
  geom_treemap() +
  scale_fill_distiller(palette = 'RdYlBu', direction = 1) +
  geom_treemap_subgroup_border(colour = 'white')+
  #subgroup heading in white
  geom_treemap_subgroup_text(color="black", size = 15, place = 'center')+
  #all other group text in black
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Countries competing at Winter Games since 1994',
       subtitle = 'Size of box represents number of competitors',
       fill = "Mean GDP per capita, 1994-2014 (US$ thousand)") +
  oly_theme +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggsave(here::here('plots', 'winter_gdp_country.png'), height = 4, width = 4 *16/9)

# Summer country tree plot
season_gdp_summary %>% 
  filter(Season == 'Summer') %>% 
  ggplot(aes(area = count, fill = gdp/10^3, subgroup = Team)) +
  geom_treemap() +
  scale_fill_distiller(palette = 'RdYlBu', direction = 1) +
  geom_treemap_subgroup_border(colour = 'white')+
  #subgroup heading in white
  geom_treemap_subgroup_text(color="black", size = 15, place = 'center')+
  #all other group text in black
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = 'Countries competing at Summer Games since 1996',
       subtitle = 'Size of box represents number of competitors',
       fill = "Mean GDP per capita, 1996-2016 (US$ thousand)") +
  oly_theme +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  ggsave(here::here('plots', 'summer_gdp_country.png'), height = 4, width = 4 *16/9)

### PLOT 4 - GDP PER SPORT 1996-2016 ------------------------------------------
### Mean GDP per capita for sports in 1996-2016
sport_allyrs_gdp <- oly %>% 
  filter(Year >=1994) %>% 
  drop_na() %>% 
  group_by(Sport, Season) %>% 
  summarise(gdp_sport = sum(gdp)/n()) %>% 
  arrange(desc(gdp_sport)) %>% 
  mutate(rank = row_number(),
         position = case_when(gdp_sport/10^3 >= 30 ~'Highest',
                              gdp_sport/10^3 >= 25 ~'Middle',
                              gdp_sport/10^3 < 25~'Lowest')) %>% 
  glimpse()

# Summer Games plot 1996-2016
sport_allyrs_gdp %>% 
  filter(Season == 'Summer') %>% 
  ggplot(aes(x = reorder(Sport, gdp_sport), y = gdp_sport/10^3)) +
  geom_point(aes(colour = position)) +
  geom_segment(aes(xend = Sport, yend = 0, colour = position)) +
  coord_flip() +
  oly_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "None",
        strip.text = element_blank(),
        panel.grid.major = element_blank()) +
  labs(title = 'Mean GDP per capita of countries by sport, All Summer Games 1996-2016',
       caption = 'GDP per capita in thousands of US dollars, 2010 nominal terms \nGDP per capita weighted by number of participants from each country',
       x = '',
       y = '') +
  scale_color_manual(values = c(oly_pal[9], oly_pal [2], oly_pal[4])) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), 
                     labels = dollar_format(suffix = "k")) +
  ggsave(here::here('plots', 'gdp_allyrs_sport.png'), height = 6, width = 4 *16/9)

# Winter Games plot 1994-2014
sport_allyrs_gdp %>% 
  filter(Season == 'Winter') %>% 
  ggplot(aes(x = reorder(Sport, gdp_sport), y = gdp_sport/10^3)) +
  geom_point(aes(colour = position)) +
  geom_segment(aes(xend = Sport, yend = 0, colour = position)) +
  coord_flip() +
  oly_theme +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "None",
        strip.text = element_blank(),
        panel.grid.major = element_blank()) +
  labs(title = 'Mean GDP per capita of countries by sport, All Winter Games 1994-2014',
       caption = 'GDP per capita in thousands of US dollars, 2010 nominal terms \nGDP per capita weighted by number of participants from each country',
       x = '',
       y = '') +
  scale_color_manual(values = c(oly_pal[9], oly_pal[4])) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), 
                     labels = dollar_format(suffix = "k")) +
  ggsave(here::here('plots', 'gdp_winter_allyrs_sport.png'), height = 4, width = 4 *16/9)
