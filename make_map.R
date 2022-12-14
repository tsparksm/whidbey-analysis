#### Setup ####
source(here::here("src", "utility_functions.R"))
library(ggmap)

locators <- load_locator_all()  # use jitter version for best map

# Change plotting order
locators$Agency <- factor(locators$Agency, 
                          levels = c("Ecology", 
                                     "Stillaguamish", 
                                     "Tulalip", 
                                     "UW-Collias", 
                                     "UW-PRISM/WOAC", 
                                     "King County"))

# Load map images from Google API
PS_map <- get_stamenmap(c(left = -122.779, 
                          bottom = 47.880, 
                          right = -122.172, 
                          top = 48.452), 
                        maptype = "toner-background", 
                        force = T, 
                        zoom = 10, 
                        scale = 2)

# increase alpha of background map to get gray instead of black
mapatt <- attributes(PS_map)
map_transparent <- matrix(adjustcolor(PS_map, alpha.f = 0.2), 
                          nrow = nrow(PS_map))
attributes(map_transparent) <- mapatt

ggmap(map_transparent) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + 
  geom_point(data = locators, 
             aes(x = Lon, y = Lat, color = Agency, shape = Agency), 
             size = 1) + 
  scale_color_viridis_d(direction = 1) +
  scale_shape_manual(values = c(15, 16, 17, 3, 4, 8)) + 
  labs(x = "", y = "", color = "", shape = "")
ggsave(here("figs", "whidbey_station_map.png"), 
       dpi = 600)
