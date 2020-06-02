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

gdp_pal <- c('#2166ac', '#4393c3', '#92c5de', '#d1e5f0',
             '#fddbc7', '#f4a582', '#d6604d', '#b2182b')


### PLOT 1 - MEDALS BY COUNTRY 2016 -------------------------------------------
# 2016 gold medals per country
medals_rio <- country_medal %>% 
  filter(Year == 2016) %>% 
  group_by(country) %>% 
  summarise(total = sum(medal_count), gdp = mean(gdp)) %>% 
  arrange(desc(total)) %>% 
  mutate(rank = row_number(),
         gdp_per_medal = total/gdp*10^3) %>% 
  arrange(desc(gdp_per_medal)) %>% 
  mutate(rank_gdp = row_number(),
         high_label = ifelse(rank_gdp == 3, 
                             paste0(total, " medals"), 
                             as.character(total)),
         gdp_flag = as_factor(case_when(gdp < 1000 ~'<1k',
                              gdp <2000 ~'1-2k',
                              gdp <5000 ~'2-5k',
                              gdp <10000 ~'5-10k',
                              gdp <15000 ~'10-15k',
                              gdp <20000 ~'15-20k',
                              gdp <40000 ~'20-40k',
                              gdp >=40000 ~'>40k')),
         gdp_flag = fct_relevel(gdp_flag, c('>40k', '20-40k', 
                                            '15-20k', '10-15k',
                                            '5-10k', '2-5k',
                                            '1-2k', '<1k'))) %>%
  glimpse()

# PLOT 2016 medal bar chart
medals_rio %>% 
  filter(rank <=20) %>% 
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col(aes(fill = gdp_flag), width = 0.6) +
  coord_flip() +
  oly_theme +
  scale_fill_manual(values = gdp_pal, drop = FALSE) +
  labs(title = "Top 20 countries at 2016 Summer Games, by medals won",
       fill = 'GDP per capita\nUS$',
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 250), breaks = seq(0, 250, 50)) +
  ggsave(here::here('plots', '2016_medals_with_gdp.png'), height = 4.5, width = 4 *16/9)

# PLOT 2016 medal normalised for GDP per capita 
medals_rio %>% 
  filter(rank_gdp <=20) %>% 
  ggplot(aes(x = reorder(country, gdp_per_medal), y = gdp_per_medal)) +
  geom_col(aes(fill = gdp_flag), width = 0.6) +
  geom_text(aes(label = high_label), hjust = 'left', nudge_y = 0.5, size = 3) +
  coord_flip() +
  oly_theme +
  scale_fill_manual(values = gdp_pal, drop = FALSE) +
  labs(title = "Top 20 countries at 2016 Summer Games",
       subtitle = "Medals won per thousands dollars of GDP per capita",
       x = "",
       y = "",
       fill = "GDP per capita\nUS$") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) +
  ggsave(here::here('plots', '2016_medals_per_gdp.png'), height = 4.5, width = 4 *16/9)


### PLOT 2 - SUMMER MEDALS BY COUNTRY 1996-2016 -------------------------------
# Medals per country aggregated 1996-2016
medals_gdp <- country_medal %>% 
  filter(Year >= 1994 & Season == 'Summer') %>% 
  group_by(country) %>% 
  summarise(total = sum(medal_count), gdp = mean(gdp)) %>% 
  arrange(desc(total)) %>% 
  filter(total >= 6*3) %>% 
  mutate(rank = row_number(),
         gdp_per_medal = total/gdp*10^3) %>% 
  arrange(desc(gdp_per_medal)) %>% 
  mutate(rank_gdp = row_number(),
         high_label = ifelse(rank_gdp == 3, 
                             paste0(total, " medals"), 
                             as.character(total)),
         gdp_flag = as_factor(case_when(gdp < 1000 ~'<1k',
                                        gdp <2000 ~'1-2k',
                                        gdp <5000 ~'2-5k',
                                        gdp <10000 ~'5-10k',
                                        gdp <15000 ~'10-15k',
                                        gdp <20000 ~'15-20k',
                                        gdp <40000 ~'20-40k',
                                        gdp >=40000 ~'>40k')),
         gdp_flag = fct_relevel(gdp_flag, c('>40k', '20-40k', 
                                            '15-20k', '10-15k',
                                            '5-10k', '2-5k',
                                            '1-2k', '<1k'))) %>%
  glimpse()

# PLOT 1996-2016 medal bar chart
medals_gdp %>% 
  filter(rank <=20) %>% 
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col(aes(fill = gdp_flag), width = 0.6) +
  coord_flip() +
  oly_theme +
  scale_fill_manual(values = gdp_pal, drop = FALSE) +
  labs(title = "Top 20 countries by medals won, all Summer Games 1996-2016",
       fill = "GDP per capita\nUS$",
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 1400), breaks = seq(0, 1400, 200)) +
  ggsave(here::here('plots', 'summer_medals_with_gdp.png'), height = 4.5, width = 4 *16/9)

# PLOT 1996-2016 medal normalised for GDP per capita 
medals_gdp %>% 
  filter(rank_gdp <=20) %>% 
  ggplot(aes(x = reorder(country, gdp_per_medal), y = gdp_per_medal)) +
  geom_col(aes(fill = gdp_flag), width = 0.6) +
  geom_text(aes(label = high_label), hjust = 'left', nudge_y = 5, size = 3) +
  coord_flip() +
  oly_theme +
  scale_fill_manual(values = gdp_pal, drop = FALSE) +
  labs(title = "Top 20 countries all Summer Games 1996-2016",
       subtitle = "Medals won per thousands dollars of GDP per capita",
       x = "",
       y = "",
       fill = "GDP per capita\nUS$") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  ggsave(here::here('plots', 'summer_medals_per_gdp.png'), height = 4.5, width = 4 *16/9)

### PLOT 3 - WINTER MEDALS BY COUNTRY 1994-2014 ----------------------------------
# Medals per country aggregated 1994-2014
winter_medals_gdp <- country_medal %>% 
  filter(Year >= 1994 & Season == 'Winter') %>% 
  group_by(country) %>% 
  summarise(total = sum(medal_count), gdp = mean(gdp)) %>% 
  arrange(desc(total)) %>% 
  filter(total >= 6*3) %>% 
  mutate(rank = row_number(),
         gdp_per_medal = total/gdp*10^3) %>% 
  arrange(desc(gdp_per_medal)) %>% 
  mutate(rank_gdp = row_number(),
         high_label = ifelse(rank_gdp == 3, 
                             paste0(total, " medals"), 
                             as.character(total)),
         gdp_flag = as_factor(case_when(gdp < 1000 ~'<1k',
                                        gdp <2000 ~'1-2k',
                                        gdp <5000 ~'2-5k',
                                        gdp <10000 ~'5-10k',
                                        gdp <15000 ~'10-15k',
                                        gdp <20000 ~'15-20k',
                                        gdp <40000 ~'20-40k',
                                        gdp >=40000 ~'>40k')),
         gdp_flag = fct_relevel(gdp_flag, c('>40k', '20-40k', 
                                            '15-20k', '10-15k',
                                            '5-10k', '2-5k',
                                            '1-2k', '<1k'))) %>%
  glimpse()

# PLOT 1996-2016 medal bar chart
winter_medals_gdp %>% 
  filter(rank <=20) %>% 
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col(aes(fill = gdp_flag), width = 0.6) +
  coord_flip() +
  oly_theme +
  scale_fill_manual(values = gdp_pal, drop = FALSE) +
  labs(title = "Top 20 countries by medals won, all Winter Games 1994-2014",
       fill = "GDP per capita\nUS$",
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 50)) +
  ggsave(here::here('plots', 'winter_medals_with_gdp.png'), height = 4.5, width = 4 *16/9)

# PLOT 1996-2016 medal normalised for GDP per capita 
winter_medals_gdp %>% 
  filter(rank_gdp <=20) %>% 
  ggplot(aes(x = reorder(country, gdp_per_medal), y = gdp_per_medal)) +
  geom_col(aes(fill = gdp_flag), width = 0.6) +
  geom_text(aes(label = high_label), hjust = 'left', nudge_y = 1, size = 3) +
  coord_flip() +
  oly_theme +
  scale_fill_manual(values = gdp_pal, drop = FALSE) +
  labs(title = "Top 20 countries all Winter Games 1996-2016",
       subtitle = "Medals won per thousands dollars of GDP per capita",
       x = "",
       y = "",
       fill = "GDP per capita\nUS$") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  ggsave(here::here('plots', 'winter_medals_per_gdp.png'), height = 4.5, width = 4 *16/9)
