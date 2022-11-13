library(tidyverse)
library(tidytext)
library(lubridate)

whatsapp <- read_csv("data/whatsapp_christian3.txt") 

# Datenbereinigung --------------------------------------------------------
whatsapp_cleaned <- whatsapp %>% 
  setNames(c("date", "post")) %>%
  # filter(date %>% str_detect("^[:digit:]{2}")) %>%
  filter(post %>% str_detect(":")) %>%
  separate(post, into = c("time", "post"), sep = " - ") %>% 
  separate(post, into = c("person", "message"), sep = ":") %>% 
  mutate(
    person = fct_anon(as_factor(person))
  ) %>% 
  mutate(
    date = dmy(date)
  ) %>% 
  dplyr::filter(!is.na(person)) %>% 
  mutate(
    datetime = paste(as.character(date), time) %>% 
      parse_date_time("y-m-d H:M"),
    month = month(date, label = TRUE),
    year = year(datetime),
    hour = hour(datetime),    
    wday = wday(date, label = TRUE, abbr = TRUE),
    laughs = str_count(message, "🤣|😅|😁")
  ) %>% 
  select(-message) 

# Wer schreibt wie oft Nachrichten ----------------------------------------
whatsapp_cleaned %>% 
  ggplot(aes(x = month)) +
  geom_bar(aes(fill = person),
           position = position_dodge()) +
  facet_wrap(vars(year))


# Wer sendet wie viele Emojis ---------------------------------------------




# Wer meldet sich am ersten am Tag ----------------------------------------
whatsapp_cleaned %>% 
  group_by(date) %>% 
  arrange(datetime) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  ggplot(aes(x = person)) + 
  geom_bar() +
  facet_wrap(vars(year))


# Wann wird geschrieben ---------------------------------------------------
hour_count <- whatsapp_cleaned %>% 
  count(hour, year) %>% 
  drop_na(year) 

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(hour_count, aes(x = hour, n)) +   
  geom_polygon(fill = "#009688", group = 1, color = "#4cb5ab", alpha = .70) +
  geom_point(color = "#99d5cf", size = 1.3, alpha = .8) +
  coord_radar() +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  # theme_minimal() +
  facet_wrap(vars(year))
