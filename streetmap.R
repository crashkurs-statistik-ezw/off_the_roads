library(tidyverse)
library(osmdata)

stadt <- getbb("Freiburg im Breisgau")

# stadt <- matrix(c(10.087509, 10.169907, 48.237194, 48.262198),
#                        nrow = 2)

streets <- stadt %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- stadt %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street")) %>% 
  # "unclassified") %>%
  # "service", "footway")) %>%
  osmdata_sf()

river <- stadt %>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "white",
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(24.78280,25.25451), 
           ylim = c(59.92249, 60.29785),
           expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey20", color = NA)
  )

ggsave("map.png", width = 6, height = 6)