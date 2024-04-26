#### Setup ####
source(here::here("src", "utility_functions.R"))
library(ggmap)
library(ggrepel)

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
PS_map <- get_stadiamap(c(left = -122.779, 
                          bottom = 47.880, 
                          right = -122.172, 
                          top = 48.452), 
                        maptype = "stamen_toner_background", 
                        force = T, 
                        zoom = 10, 
                        scale = 2)

# increase alpha of background map to get gray instead of black
mapatt <- attributes(PS_map)
map_transparent <- matrix(adjustcolor(PS_map, alpha.f = 0.2), 
                          nrow = nrow(PS_map))
attributes(map_transparent) <- mapatt

#### Map with all agencies' stations ####
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

#### Map with just KC stations ####
data_to_plot = locators %>% 
  filter(Agency == "King County") %>% 
  mutate(Shape = case_when(
    Name == "PENNCOVEPNN001" ~ "discontinued", 
    Name == "Poss DO-2" ~ "added", 
    Data_Type == "CTD" ~ "CTD", 
    TRUE ~ "CTD+bottle"))

g <- ggmap(map_transparent) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none") + 
  geom_point(data = data_to_plot, 
             aes(x = Lon, 
                 y = Lat, 
                 shape = Shape, 
                 color = Has_Mooring), 
             size = 2) + 
  geom_text_repel(data = data_to_plot, 
                  aes(x = Lon, 
                      y = Lat, 
                      color = Has_Mooring, 
                      label = Name), 
                  box.padding = 0.3, 
                  xlim = c(NA, Inf), 
                  ylim = c(-Inf, Inf), 
                  min.segment.length = 0) + 
  coord_cartesian(clip = "off") + 
  scale_shape_manual(values = c("discontinued" = 4, 
                                "added" = 2, 
                                "CTD" = 17, 
                                "CTD+bottle" = 16)) + 
  scale_color_manual(values = c("TRUE" = "red", 
                                "FALSE" = "black")) + 
  labs(x = "", y = "", shape = "")
ggsave(here("figs", "whidbey_station_map_KC.png"), g, 
       dpi = 600, 
       height = 5, width = 4)

#### Map with just KC stations - no text ####
data_to_plot = locators %>% 
  filter(Agency == "King County", 
         is.na(Last_Year))

g <- ggmap(map_transparent) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = 'white'), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none") + 
  geom_point(data = data_to_plot, 
             aes(x = Lon, 
                 y = Lat, 
                 shape = Data_Type, 
                 color = Has_Mooring), 
             size = 4) + 
  coord_cartesian(clip = "off") + 
  scale_shape_manual(values = c("CTD" = 17, 
                                "CTD, bottle" = 16)) + 
  scale_color_manual(values = c("TRUE" = "red", 
                                "FALSE" = "black")) + 
  labs(x = "", y = "", shape = "")
ggsave(here("figs", "whidbey_station_map_KC_notext.png"), g, 
       dpi = 600, 
       height = 5, width = 4)
