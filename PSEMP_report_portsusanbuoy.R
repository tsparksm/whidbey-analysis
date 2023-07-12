#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(metR)
library(cmocean)

yoi <- 2022

# Figure settings
fig_dpi <- 600
fig_height <- 12
fig_width <- 16
font_size <- 12

# Plot settings
palette_mooring <- c("#bebebe", "black")
shapes_mooring <- c(8, 16)
shapes_mdl <- c(16, 1)
glob_lab_scale <- 5/14
point_size <- 5

# Contour plot settings
acc_T <- 0.2
acc_S <- 0.1
acc_DO <- 0.2
acc_chl <- 0.5
acc_N <- 0.05

#### Load data ####
data_discrete <- load_whidbey_discrete() %>% 
  mutate(Type = "bottle", 
         DateTime = CollectDateTime) %>% 
  filter(Locator == "PSUSANBUOY")

data_buoy <- read_csv(here("data", "port_susan_buoy_qc.csv")) %>% 
  mutate(DateTime = DateTime - 8*60*60, 
         Year = year(DateTime)) %>% 
  filter(Year <= yoi, 
         !(Flag %in% 3:4)) %>% 
  mutate(YearGroup = ifelse(year(DateTime) == yoi, 
                            yoi, 
                            paste(min(year(DateTime)), 
                                  yoi-1, 
                                  sep = "-")), 
         FakeDate = DateTime) %>% 
  arrange(DateTime)
year(data_buoy$FakeDate) <- yoi
max_date <- max(data_buoy$FakeDate, na.rm = TRUE)
min_date <- min(data_buoy$FakeDate, na.rm = TRUE)

bin_width <- 0.5
data_ctd <- load_composite(bin_width, 
                           monthly = FALSE) %>% 
  filter(Locator == "PSUSANBUOY") %>% 
  mutate(YearDay = yday(Date), 
         Year = year(Date))

# Add extra CTD data before/after each year
extra_data_before <- data_ctd %>% 
  filter(Year == yoi - 1, 
         YearDay == max(YearDay)) %>% 
  mutate(YearDay = YearDay - 365, 
         Year = yoi)
data_ctd <- add_row(data_ctd, extra_data_before)

extra_data_after <- data_ctd %>% 
  filter(Year == yoi + 1, 
         YearDay == min(YearDay)) %>% 
  mutate(YearDay = YearDay + 365, 
         Year = yoi)
data_ctd <- add_row(data_ctd, extra_data_after) %>% 
  filter(Year == yoi)

#### T contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(Temperature)) %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(Temperature = mean(Temperature)) %>% 
  ungroup()

min_lim <- round_any(min(data_to_plot$Temperature), 
                     accuracy = acc_T, 
                     f = floor)
max_lim <- round_any(max(data_to_plot$Temperature), 
                     accuracy = acc_T, 
                     f = ceiling)
mybreaks <- seq(min_lim, max_lim, by = acc_T)
mylabels <- mybreaks
mylabels[mylabels %% 5 != 0] <- ""

p1 <- ggplot(data = data_to_plot) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank()) + 
  metR::geom_contour_fill(aes(x = YearDay, 
                              y = BinDepth, 
                              z = Temperature), 
                          na.fill = TRUE, 
                          breaks = mybreaks) + 
  scale_fill_cmocean(name = "thermal", 
                     breaks = mybreaks, 
                     limits = c(min_lim, max_lim), 
                     labels = mylabels, 
                     guide = guide_colorbar(show.limits = T, ticks = F)) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(0, 366)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                yday(paste(yoi, "-02-01", sep = "")), 
                                yday(paste(yoi, "-03-01", sep = "")), 
                                yday(paste(yoi, "-04-01", sep = "")), 
                                yday(paste(yoi, "-05-01", sep = "")), 
                                yday(paste(yoi, "-06-01", sep = "")), 
                                yday(paste(yoi, "-07-01", sep = "")), 
                                yday(paste(yoi, "-08-01", sep = "")), 
                                yday(paste(yoi, "-09-01", sep = "")), 
                                yday(paste(yoi, "-10-01", sep = "")), 
                                yday(paste(yoi, "-11-01", sep = "")), 
                                yday(paste(yoi, "-12-01", sep = ""))), 
                     labels = month.abb) + 
  geom_vline(aes(xintercept = YearDay), 
             alpha = 0.2) + 
  labs(x = "", 
       y = "", 
       fill = "\u00B0C", 
       title = "A. Temperature")

#### S contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(Salinity)) %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(Salinity = mean(Salinity)) %>% 
  ungroup()

min_lim <- round_any(min(data_to_plot$Salinity), 
                     accuracy = acc_S, 
                     f = floor)
max_lim <- round_any(max(data_to_plot$Salinity), 
                     accuracy = acc_S, 
                     f = ceiling)
mybreaks <- seq(min_lim, max_lim, by = acc_S)
mylabels <- mybreaks
mylabels[round(mylabels %% 5, 1) != 0] <- ""

p2 <- ggplot(data = data_to_plot) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank()) + 
  metR::geom_contour_fill(aes(x = YearDay, 
                              y = BinDepth, 
                              z = Salinity), 
                          na.fill = TRUE, 
                          breaks = mybreaks) + 
  scale_fill_cmocean(name = "haline", 
                     breaks = mybreaks, 
                     limits = c(min_lim, max_lim), 
                     labels = mylabels, 
                     guide = guide_colorbar(show.limits = T, 
                                            ticks = F, 
                                            reverse = T)) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(0, 366)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                yday(paste(yoi, "-02-01", sep = "")), 
                                yday(paste(yoi, "-03-01", sep = "")), 
                                yday(paste(yoi, "-04-01", sep = "")), 
                                yday(paste(yoi, "-05-01", sep = "")), 
                                yday(paste(yoi, "-06-01", sep = "")), 
                                yday(paste(yoi, "-07-01", sep = "")), 
                                yday(paste(yoi, "-08-01", sep = "")), 
                                yday(paste(yoi, "-09-01", sep = "")), 
                                yday(paste(yoi, "-10-01", sep = "")), 
                                yday(paste(yoi, "-11-01", sep = "")), 
                                yday(paste(yoi, "-12-01", sep = ""))), 
                     labels = month.abb) + 
  geom_vline(aes(xintercept = YearDay), 
             alpha = 0.2) + 
  labs(x = "", 
       y = "", 
       fill = "PSU", 
       title = "B. Salinity")

#### DO contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(DO)) %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(DO = mean(DO)) %>% 
  ungroup()

min_lim <- round_any(min(data_to_plot$DO), 
                     accuracy = acc_DO, 
                     f = floor)
max_lim <- round_any(max(data_to_plot$DO), 
                     accuracy = acc_DO, 
                     f = ceiling)
mybreaks <- seq(min_lim, max_lim, by = acc_DO)
mylabels <- mybreaks
mylabels[round(mybreaks %% 2, 1) != 0] <- ""

p3 <- ggplot(data = data_to_plot) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank()) + 
  metR::geom_contour_fill(aes(x = YearDay, 
                              y = BinDepth, 
                              z = DO), 
                          na.fill = TRUE, 
                          breaks = mybreaks) + 
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "PuBu", 
    direction = -1, 
    limits = c(min_lim, max_lim), 
    labels = mylabels, 
    guide = guide_colorbar(show.limits = T, ticks = F)) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(0, 366)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                yday(paste(yoi, "-02-01", sep = "")), 
                                yday(paste(yoi, "-03-01", sep = "")), 
                                yday(paste(yoi, "-04-01", sep = "")), 
                                yday(paste(yoi, "-05-01", sep = "")), 
                                yday(paste(yoi, "-06-01", sep = "")), 
                                yday(paste(yoi, "-07-01", sep = "")), 
                                yday(paste(yoi, "-08-01", sep = "")), 
                                yday(paste(yoi, "-09-01", sep = "")), 
                                yday(paste(yoi, "-10-01", sep = "")), 
                                yday(paste(yoi, "-11-01", sep = "")), 
                                yday(paste(yoi, "-12-01", sep = ""))), 
                     labels = month.abb) + 
  geom_vline(aes(xintercept = YearDay), 
             alpha = 0.2) + 
  labs(x = "", 
       y = "", 
       fill = "mg/L", 
       title = "C. Dissolved oxygen")

#### Chl contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(Chlorophyll)) %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(Chlorophyll = mean(Chlorophyll)) %>% 
  ungroup()

min_lim <- round_any(min(data_to_plot$Chlorophyll), 
                     accuracy = acc_chl, 
                     f = floor)
max_lim <- round_any(max(data_to_plot$Chlorophyll), 
                     accuracy = acc_chl, 
                     f = ceiling)
mybreaks <- seq(min_lim, max_lim, by = acc_chl)
mylabels <- mybreaks
mylabels[round(mybreaks %% 4, 1) != 0] <- ""

p4 <- ggplot(data = data_to_plot) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank()) + 
  metR::geom_contour_fill(aes(x = YearDay, 
                              y = BinDepth, 
                              z = Chlorophyll), 
                          na.fill = TRUE, 
                          breaks = mybreaks) + 
  scale_fill_cmocean(name = "algae", 
                     breaks = mybreaks, 
                     limits = c(min_lim, max_lim), 
                     labels = mylabels, 
                     guide = guide_colorbar(show.limits = T, ticks = F)) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(0, 366)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                yday(paste(yoi, "-02-01", sep = "")), 
                                yday(paste(yoi, "-03-01", sep = "")), 
                                yday(paste(yoi, "-04-01", sep = "")), 
                                yday(paste(yoi, "-05-01", sep = "")), 
                                yday(paste(yoi, "-06-01", sep = "")), 
                                yday(paste(yoi, "-07-01", sep = "")), 
                                yday(paste(yoi, "-08-01", sep = "")), 
                                yday(paste(yoi, "-09-01", sep = "")), 
                                yday(paste(yoi, "-10-01", sep = "")), 
                                yday(paste(yoi, "-11-01", sep = "")), 
                                yday(paste(yoi, "-12-01", sep = ""))), 
                     labels = month.abb) + 
  geom_vline(aes(xintercept = YearDay), 
             alpha = 0.2) + 
  labs(x = "", 
       y = "", 
       fill = expression(mu*g/L), 
       title = "D. Chlorophyll fluorescence")

#### N contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(NO23)) %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(NO23 = mean(NO23)) %>% 
  ungroup()

min_lim <- round_any(min(data_to_plot$NO23), 
                     accuracy = acc_N, 
                     f = floor)
max_lim <- round_any(max(data_to_plot$NO23), 
                     accuracy = acc_N, 
                     f = ceiling)
mybreaks <- seq(min_lim, max_lim, by = acc_N)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 1), 2))] <- ""

p5 <- ggplot(data = data_to_plot) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_text(size = font_size + 2, face = "bold")) + 
  metR::geom_contour_fill(aes(x = YearDay, 
                              y = BinDepth, 
                              z = NO23), 
                          na.fill = TRUE, 
                          breaks = mybreaks) + 
  scale_fill_craftfermenter(
    breaks = mybreaks, 
    palette = "YlOrRd", 
    direction = 1, 
    limits = c(min_lim, max_lim), 
    labels = mylabels, 
    guide = guide_colorbar(show.limits = T, ticks = F)) + 
  scale_y_reverse(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(0, 366)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(yday(paste(yoi, "-01-01", sep = "")), 
                                yday(paste(yoi, "-02-01", sep = "")), 
                                yday(paste(yoi, "-03-01", sep = "")), 
                                yday(paste(yoi, "-04-01", sep = "")), 
                                yday(paste(yoi, "-05-01", sep = "")), 
                                yday(paste(yoi, "-06-01", sep = "")), 
                                yday(paste(yoi, "-07-01", sep = "")), 
                                yday(paste(yoi, "-08-01", sep = "")), 
                                yday(paste(yoi, "-09-01", sep = "")), 
                                yday(paste(yoi, "-10-01", sep = "")), 
                                yday(paste(yoi, "-11-01", sep = "")), 
                                yday(paste(yoi, "-12-01", sep = ""))), 
                     labels = month.abb) + 
  geom_vline(aes(xintercept = YearDay), 
             alpha = 0.2) + 
  labs(x = "", 
       y = "", 
       fill = "mg N/L", 
       title = "E. Nitrate + nitrite")

#### T mooring ####
data_to_plot <- data_buoy %>% 
  filter(Parameter == "Temperature")

p6 <- ggplot(data = data_to_plot, 
       aes(x = FakeDate, 
           y = Value, 
           color = YearGroup, 
           shape = Type)) + 
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = font_size), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank()) + 
  geom_point() + 
  scale_color_manual(values = palette_mooring) + 
  scale_shape_manual(values = shapes_mooring) + 
  scale_x_datetime(date_breaks = "1 month", 
                   limits = c(min_date, max_date), 
                   expand = c(0, 0)) + 
  labs(x = "", 
       y = "(\u00B0C)", 
       title = "F. Surface temperature")

#### S mooring ####
data_to_plot <- data_buoy %>% 
  filter(Parameter == "Salinity")

p7 <- ggplot(data = data_to_plot, 
             aes(x = FakeDate, 
                 y = Value, 
                 color = YearGroup, 
                 shape = Type)) + 
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = font_size), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank()) + 
  geom_point() + 
  scale_color_manual(values = palette_mooring) + 
  scale_shape_manual(values = shapes_mooring) + 
  scale_x_datetime(date_breaks = "1 month", 
                   limits = c(min_date, max_date), 
                   expand = c(0, 0)) + 
  labs(x = "", 
       y = "(PSU)", 
       title = "G. Surface salinity")

#### DO mooring ####
data_to_plot <- data_buoy %>% 
  filter(Parameter == "Oxygen")

p8 <- ggplot(data = data_to_plot, 
             aes(x = FakeDate, 
                 y = Value, 
                 color = YearGroup, 
                 shape = Type)) + 
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = font_size), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank()) + 
  geom_point() + 
  scale_color_manual(values = palette_mooring) + 
  scale_shape_manual(values = shapes_mooring) + 
  scale_x_datetime(date_breaks = "1 month", 
                   limits = c(min_date, max_date), 
                   expand = c(0, 0)) + 
  labs(x = "", 
       y = "(mg/L)", 
       title = "H. Surface dissolved oxygen")

#### Chl mooring ####
data_to_plot <- data_buoy %>% 
  filter(Parameter == "Chlorophyll")

p9 <- ggplot(data = data_to_plot, 
             aes(x = FakeDate, 
                 y = Value, 
                 color = YearGroup, 
                 shape = Type)) + 
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = font_size), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank()) + 
  geom_point() + 
  scale_color_manual(values = palette_mooring) + 
  scale_shape_manual(values = shapes_mooring) + 
  scale_x_datetime(date_breaks = "1 month", 
                   limits = c(min_date, max_date), 
                   expand = c(0, 0)) + 
  labs(x = "", 
       y = expression((mu*g/L)), 
       title = "I. Surface chlorophyll fluorescence")

#### N bottle ####
data_to_plot <- data_discrete %>% 
  filter(ParmId == 14, 
         DepthBin == "surface", 
         year(DateTime) == yoi)

p10 <- ggplot(data = data_to_plot, 
              aes(x = CollectDateTime, 
                  y = Value, 
                  shape = grepl("MDL", QfrCode))) + 
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = font_size), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size = font_size + 2, 
                                   face = "bold")) + 
  geom_point(size = point_size) + 
  scale_shape_manual(values = shapes_mdl) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b", 
                   expand = c(0, 0), 
                   limits = c(min_date, max_date)) + 
  labs(x = "", 
       y = "(mg N/L)", 
       title = "J. Surface nitrate + nitrite")

#### Put it all together ####
glob_lab <- "Depth (m)"
p_lab <- 
  ggplot() + 
  annotate(geom = "text", 
           x = 1, y = 1, 
           label = glob_lab, 
           angle = 90, 
           size = (font_size + 4)*glob_lab_scale) + 
  coord_cartesian(clip = "off") + 
  theme_void()
pp <- (p_lab | (((p1 / p2 / p3 / p4 / p5) | (p6 / p7 / p8 / p9 / p10)) + 
                  plot_layout(tag_level = "new"))) + 
  plot_layout(widths = c(0.05, 1))
ggsave(here("figs", paste0("PSEMP_portsusanbuoy_", yoi, ".png")), 
       pp, 
       dpi = fig_dpi, 
       height = fig_height, 
       width = fig_width)