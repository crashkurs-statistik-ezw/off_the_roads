library(tidyverse)
library(skimr)

world_bank <- read_csv("data/world_bank_countries_real.csv")


# Datenexploration --------------------------------------------------------
world_bank %>% glimpse

# Wie viele Länder gibt es?
world_bank %>% 
  count(country)

world_bank$country %>% unique

# Wie viele Kontinente gibt es
world_bank %>% 
  count(continent)

# Welche Jahre existieren im Datensatz?
world_bank$year %>% unique

# Welche Kennwerte ergeben sich für Deutschland
world_bank %>% 
  dplyr::filter(country == "Germany") %>% 
  skim

world_bank %>% 
  dplyr::filter(country %in% c("Hungary", "United States", 
  "France", "Germany")) %>% 
  group_by(country, year) %>% 
  summarise(
    across(
      .cols = access_to_electricity:renewable_energie_consumption, 
      .fns  = list(
        mean = ~ mean(., na.rm = TRUE)
        )
      )
  ) %>% view

# Mittelwerte population_density berechnen
world_bank %>% 
  group_by(continent) %>% 
  summarise(mean = mean(population_density, na.rm = TRUE))

# Gib mir die deskriptiven Werte pro Kontinent
world_bank %>% 
  group_by(continent) %>% 
  skim



# Fragen ------------------------------------------------------------------

# Wie hat sich die life_expectancy pro Kontinent entwickelt
world_bank %>% 
  group_by(year, continent) %>% 
  summarise(mean_life_expect = mean(life_expectancy_at_birth, 
                                    na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_life_expect)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(1960, 2022, 20)) +
  facet_wrap(vars(continent))

smallest_life_exp <- world_bank %>% 
  arrange(life_expectancy_at_birth) %>% 
  select(year, country, continent, life_expectancy_at_birth) %>% 
  head(n = 50)

countries_with_smallest_life_exp <- smallest_life_exp$country %>% unique

world_bank %>% 
  dplyr::filter(country %in% countries_with_smallest_life_exp) %>% 
  ggplot(aes(x = year, y = life_expectancy_at_birth,
             color = country)) + 
  geom_line() +
  facet_wrap(vars(country))


# Wie hat sich die Geburtenrate über alle Kontinente
# über die Zeit entwickelt?
world_bank %>% 
  group_by(year, continent) %>% 
  summarise(mean_birth_rate = mean(birth_rate, 
                                    na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_birth_rate)) + 
  geom_line(data = world_bank, 
            aes(x = year, y = birth_rate, group = country), 
            color = "grey80", alpha = .5) +
  geom_line() +

  scale_x_continuous(breaks = seq(1960, 2022, 20)) +
  facet_wrap(vars(continent))

world_bank %>% 
  dplyr::filter(country == "China") %>% 
  ggplot(aes(x = year, y = birth_rate)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(1960, 2022, 5)) 

# Zusammenhang overweight und life expectancy der Länder im Vergleich
world_bank %>% 
  # dplyr::filter(year == 2000) %>% 
  mutate(
    decade = (year = 10 * (year %/% 10))
  ) %>% 
  ggplot(aes(x = overweight, y = life_expectancy_at_birth,
             color = gdp_per_capita)) + 
  geom_point(alpha = .1) +
  facet_wrap(vars(decade)) +
  scale_color_gradient2(low = '#009392', mid = '#f6edbd', high = '#d0587e', midpoint = 5)

# Zusammenhang overweight und co2 ausstoß der Länder im Vergleich
world_bank %>% 
  dplyr::filter(year == 2000) %>%
  ggplot(aes(x = overweight, y = co2_emissions_tons_per_capita)) + 
  geom_point(alpha = .9) +
  ylim(c(0, 60)) 
  # coord_cartesian(ylim = c(0, 60))

# Was sind die Länder mit dem größten Übergewicht
world_bank %>% 
  dplyr::filter(year == 2014) %>% 
  slice_min(overweight, n = 50) %>% 
  select(country, continent, year, overweight) %>% 
  print(n = 10)

# Co2 Austausch und Renewable Energy Consumption
world_bank %>% 
  dplyr::filter(year == 2015) %>% 
  mutate(
    co2_intervals = cut_width(co2_emissions_tons_per_capita, 10),
    high_or_low_co2 = co2_emissions_tons_per_capita > 
      mean(co2_emissions_tons_per_capita, na.rm = TRUE)
  ) %>% 
  # select(country, continent, year, co2_intervals, high_or_low_co2) %>%
  ggplot(aes(x = continent, y = renewable_energie_consumption,
             fill = co2_intervals)) +
  geom_jitter(width = .2, alpha = .1) +
  geom_boxplot(alpha = .7)
  # coord_cartesian(ylim = c(0, 10))

# Zusammenhang Internetnutzung und mobile Telefonie
world_bank %>% 
  ggplot(aes(x = year, y = internet_usage)) + 
  stat_summary(
    geom = "bar",
    fun = "mean",
    alpha = .2,
    fill = "grey80",
    color = "grey20"
  ) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = .2) +
  xlim(c(1990, 2018))

world_bank %>% 
  dplyr::filter(year == 2018) %>% 
  # mutate(
  #   mobile_subs = cut_width(mobile_cellular_subscriptions, 50)
  # ) %>% 
  ggplot(aes(x = continent, y = internet_usage)) +
  stat_summary(
    geom = "col",
    fun = "mean",
    alpha = .7,
    fill = "grey20"
  ) +
  stat_summary(
    geom = "errorbar",
    fun.data = mean_sdl,
    width = .2
  ) 
  # facet_wrap(vars(mobile_subs))
               

# Wie verteilen Frauen in den weltweiten Parliaments
top_women_in_parl_countries <- world_bank %>% 
  dplyr::filter(year == 2018) %>% 
  slice_min(women_in_national_parliaments, n = 20) %>% 
  select(country, continent, women_in_national_parliaments)

world_bank %>% 
  dplyr::filter(country%in% unique(top_women_in_parl_countries$country)) %>% 
  ggplot(aes(x = year, y = women_in_national_parliaments)) +
  geom_line() +
  facet_wrap(vars(country))
  xlim(c(1990, 2022))

world_bank %>% 
  dplyr::filter(country == "Sweden") %>% 
  ggplot(aes(x = year, y = women_in_national_parliaments)) +
  geom_line() +
  # scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  xlim(c(1990, 2022)) 


# Steht female_to_male_labor_force mit women_in_national_parliaments 
# im Zusammenhanb

world_bank %>% 
  ggplot(aes(x = year, y = forest_land)) +
  stat_summary(
    geom = "line",
    fun = "mean",
    aes(color = continent)
  ) +
  coord_cartesian(ylim = c(0, 60))

world_bank %>% 
  ggplot(aes(x = year, y = forest_land)) + 
  geom_line(aes(group = country), alpha = .2) +
  scale_x_continuous(breaks = seq(1980, 2020, 3))

world_bank %>% 
  dplyr::filter(year < 1992, forest_land > 50,
         forest_land < 55)

