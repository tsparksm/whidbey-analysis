#### Setup ####
source(here::here("src", "utility_functions.R"))
library(ggmap)
library(ggrepel)
library(ggspatial)

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

#### Map with KC stations (year 1 tech memo) ####
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

#### Map with KC stations (year 2 tech memo) ####
data_to_plot = locators %>% 
  filter(Agency == "King County",
         Name != "PENNCOVEPNN001") %>% 
  mutate(Shape = case_when(
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
  scale_shape_manual(values = c("CTD" = 17, 
                                "CTD+bottle" = 16)) + 
  scale_color_manual(values = c("TRUE" = "red", 
                                "FALSE" = "black")) + 
  labs(x = "", y = "", shape = "")
ggsave(here("figs", "whidbey_station_map_KC_y2tm.png"), g, 
       dpi = 600, 
       height = 5, width = 4)


# Map with KC stations (year 4 tech memo) ---------------------------------
data_to_plot = locators %>% 
  filter(Agency == "King County",
         Name != "PENNCOVEPNN001") %>% 
  mutate(Shape = case_when(
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
                  bg.color = "white", 
                  bg.r = 0.1, 
                  xlim = c(NA, Inf), 
                  ylim = c(-Inf, Inf), 
                  min.segment.length = 0) + 
  coord_cartesian(clip = "off") + 
  scale_shape_manual(values = c("CTD" = 17, 
                                "CTD+bottle" = 16)) + 
  scale_color_manual(values = c("TRUE" = "red", 
                                "FALSE" = "black")) + 
  labs(x = "", y = "", shape = "")
ggsave(here("figs", "whidbey_station_map_KC_y4tm.png"), g, 
       dpi = 600, 
       height = 5, width = 4)


#### Map with KC stations - no text ####
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

#### Map - offshore SAP ####
data_to_plot <- locators |> filter(Agency == "King County")

g <- ggmap(map_transparent) + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    panel.background = element_rect(fill = 'white'), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = "none"
  ) + 
  geom_point(
    data = data_to_plot, 
    aes(x = Lon, y = Lat, shape = is.na(Last_Year)), 
    size = 2
  ) + 
  geom_text_repel(
    data = data_to_plot, 
    aes(x = Lon, 
        y = Lat, 
        label = Name), 
    bg.color = "white", 
    bg.r = 0.1, 
    box.padding = 0.3, 
    xlim = c(NA, Inf), 
    ylim = c(-Inf, Inf), 
    min.segment.length = 0
  ) + 
  coord_cartesian(clip = "off") + 
  labs(x = "", y = "") + 
  annotate(
    "text", x = -122.615, y = 48.318, 
    label = "Whidbey Island", 
    fontface = "italic", 
    size = 3
  ) + 
  annotate(
    "text", x = -122.5, y = 48.2, 
    label = "Camano\nIsland", 
    lineheight = 0.8, 
    fontface = "italic", 
    size = 3
  ) + 
  scale_shape_manual(values = c(8, 16))
ggsave(
  here("figs", "whidbey_station_map_offshore_sap.png"), 
  g, dpi = 600, height = 5, width = 4
)

#### Map - mooring SAP ####
data_to_plot <- locators |>
  filter(
    Name %in% c("PENNCOVEENT", "PENNCOVECW", "PSUSANBUOY")
  )

g <- ggmap(map_transparent) + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    panel.background = element_rect(fill = 'white'), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = "none"
  ) + 
  geom_point(data = data_to_plot, aes(x = Lon, y = Lat), size = 2) + 
  annotate(
    "text", x = -122.615, y = 48.318, 
    label = "Whidbey Island", 
    fontface = "italic", 
    size = 3
  ) + 
  annotate(
    "text", x = -122.49, y = 48.225, 
    label = "Camano\nIsland", 
    lineheight = 0.8, 
    fontface = "italic", 
    size = 3
  ) + 
  geom_text_repel(
    data = data_to_plot, 
    aes(x = Lon, 
        y = Lat, 
        label = Name), 
    bg.color = "white", 
    bg.r = 0.1, 
    box.padding = 0.3, 
    xlim = c(NA, Inf), 
    ylim = c(-Inf, Inf), 
    min.segment.length = 0
  ) + 
  coord_cartesian(clip = "off") + 
  labs(x = "", y = "")

ggsave(
  here("figs", "whidbey_station_map_moorings.png"), 
  g, dpi = 600, height = 5, width = 4
)

# Map - phyto SAP ---------------------------------------------------------
label_whidbey_x <- -122.615
label_whidbey_y <- 48.318
label_camano_x <- -122.5
label_camano_y <- 48.2
data_to_plot <- locators |>
  filter(Agency == "King County", grepl("bottle", Data_Type)) |>
  add_row(Name = "", Lat = label_whidbey_y, Lon = label_whidbey_x) |>
  add_row(Name = "", Lat = label_camano_y, Lon = label_camano_x)
g <- ggmap(map_transparent) + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    panel.background = element_rect(fill = 'white'), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = "inside", 
    legend.position.inside = c(0.8, 0.9), 
    legend.background = element_rect(color = "black"), 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0, "mm"), 
    legend.spacing.y = unit(0, "mm")
  ) + 
  geom_point(
    data = data_to_plot, 
    aes(x = Lon, y = Lat, shape = is.na(Last_Year), size = is.na(Agency)), 
  ) + 
  annotate(
    "text", x = label_whidbey_x, y = label_whidbey_y, 
    label = "Whidbey Island", 
    fontface = "italic", 
    size = 3
  ) + 
  annotate(
    "text", x = label_camano_x, y = label_camano_y, 
    label = "Camano\nIsland", 
    lineheight = 0.8, 
    fontface = "italic", 
    size = 3
  ) + 
  geom_text_repel(
    data = data_to_plot, 
    aes(x = Lon, 
        y = Lat, 
        label = Name), 
    bg.color = "white", 
    bg.r = 0.1, 
    box.padding = 0.3, 
    force = 2, 
    force_pull = 0, 
    point.padding = 0.1, 
    xlim = c(NA, Inf), 
    ylim = c(-Inf, Inf), 
    min.segment.length = 0
  ) + 
  coord_cartesian(clip = "off") + 
  labs(x = "", y = "", shape = "") + 
  scale_shape_manual(values = c(8, 16), labels = c("inactive", "active")) + 
  scale_size_manual(values = c(2, 0)) + 
  guides(size = "none") + 
  annotation_scale(location = "bl") + 
  coord_sf(crs = 4326) + 
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_nautical()
  )
ggsave(
  here("figs", "whidbey_station_map_phyto_sap.png"), 
  g, dpi = 600, height = 5.5, width = 4
)

