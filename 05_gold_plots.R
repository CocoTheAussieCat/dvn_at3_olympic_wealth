source("00_packages.R")

if (!exists("oly")) {
  oly <- readRDS(here("tidy", "oly.rds"))
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


### PLOT 1: 2016 GOLD MEDALS BY COUNTRY ---------------------------------------
# Group by event and games to count gold medals per event, rather than per person
# Eg: Baseball gold medal = 1 rather than 24 (for all team members)
oly_gold <- oly %>%
  filter(Medal == 'Gold') %>% 
  group_by(country, Year, Season, Event, host_games, gdp) %>% 
  summarise(gold_medals = n()) %>% 
  mutate(gold_medals = 1) %>% 
  glimpse()

# 2016 gold medals per country
gold_rio <- oly_gold %>% 
  filter(Year == 2016) %>% 
  group_by(country) %>% 
  summarise(gold_medals = sum(gold_medals), gdp = mean(gdp)) %>% 
  arrange(desc(gold_medals)) %>% 
  mutate(rank = row_number(),
         gdp_per_gold = gold_medals/gdp*10^3) %>% 
  arrange(desc(gdp_per_gold)) %>% 
  mutate(rank_gdp = row_number()) %>% 
  glimpse()

# Summer gold medal bar chart
gold_rio %>% 
  filter(rank <=20) %>% 
  ggplot(aes(x = reorder(country, gold_medals), y = gold_medals)) +
  geom_col(aes(fill = gdp/10^3), width = 0.6) +
  coord_flip() +
  oly_theme +
  scale_fill_distiller(palette = 'RdYlBu', direction = 1) +
  labs(title = "Top 20 countries at 2016 Summer Games, by gold medals won",
       fill = "GDP per capita US$ thousand",
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +
  ggsave(here::here('plots', 'gold_with_gdp.png'), height = 6, width = 4 *16/9)

# 2016 gdp per gold medal bar chart
gold_rio %>% 
  filter(gold_medals > 3) %>% 
  ggplot(aes(x = reorder(country, gdp_per_gold), y = gdp_per_gold)) +
  geom_col(aes(fill = gdp/10^3), width = 0.6) +
  coord_flip() +
  oly_theme +
  scale_fill_distiller(palette = 'RdYlBu', direction = 1) +
  labs(title = "Top 20 countries at 2016 Summer Games",
       subtitle = "Gold medals won per thousands dollars of GDP per capita",
       x = "",
       y = "",
       fill = "GDP per capita US$ thousand") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.direction = "horizontal") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  ggsave(here::here('plots', 'gold_per_gdp.png'), height = 6, width = 4 *16/9)

