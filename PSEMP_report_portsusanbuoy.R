#### SETUP ####
source(here::here("src", "utility_functions.R"))
source(here("src", "contour_functions.R"))
library(metR)
library(cmocean)

yoi <- 2025

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
acc_chl <- 0.2
acc_N <- 0.01

#### Load data ####
data_discrete <- load_whidbey_discrete() %>% 
  mutate(Type = "bottle", 
         DateTime = CollectDateTime) %>% 
  filter(Locator == "PSUSANBUOY")

data_buoy <- load_qc_psusan() |>
  filter(Year <= yoi) |> 
  mutate(
    YearGroup = ifelse(
      year(DateTime) == yoi, 
      yoi, 
      paste(min(year(DateTime)), yoi - 1, sep = "-")), 
    FakeDate = DateTime
  ) |> 
  arrange(DateTime)
year(data_buoy$FakeDate) <- yoi
max_date <- max(data_buoy$FakeDate, na.rm = TRUE)
min_date <- min(data_buoy$FakeDate, na.rm = TRUE)

bin_width <- 0.5
data_ctd <- load_composite(bin_width, monthly = FALSE) |> 
  filter(Locator == "PSUSANBUOY") |> 
  mutate(
    YearDay = yday(Date), 
    Year = year(Date)
  )

# Add extra CTD data before/after each year
extra_data_before <- data_ctd |> 
  filter(
    Year == yoi - 1, 
    Locator == "PSUSANBUOY"
  ) |> 
  filter(YearDay == max(YearDay)) |> 
  mutate(
    YearDay = YearDay - 365, 
    Year = yoi
  )
data_ctd <- add_row(data_ctd, extra_data_before)

extra_data_after <- data_ctd |> 
  filter(
    Year == yoi + 1, 
    Locator == "PSUSANBUOY"
  ) |>  
  filter(YearDay == min(YearDay)) |> 
  mutate(
    YearDay = YearDay + 365, 
    Year = yoi
  )
data_ctd <- add_row(data_ctd, extra_data_after) |> 
  filter(Year == yoi)

#### T contour ####
data_to_plot <- data_ctd |> 
  filter(!is.na(Temperature)) |> 
  group_by(Locator, Year, YearDay, BinDepth) |> 
  summarize(Temperature = mean(Temperature)) |> 
  ungroup() |> 
  rename(FakeYearDay = YearDay)

lims <- get_limits(data_to_plot$Temperature, acc_T)
mybreaks <- seq(lims[1], lims[2], by = acc_T)
mylabels <- get_labels(mybreaks, even_only = TRUE)

p1 <- ggplot(data = data_to_plot) + 
  add_t_contour() + 
  labs(
    title = "A. Temperature", 
    fill = expression(degree*C)
  ) + 
  theme(
    text = element_text(size = font_size), 
    axis.text.x = element_blank()
  )

#### S contour ####
data_to_plot <- data_ctd |> 
  filter(!is.na(Salinity)) |> 
  group_by(Locator, Year, YearDay, BinDepth) |> 
  summarize(Salinity = mean(Salinity)) |> 
  ungroup() |> 
  rename(FakeYearDay = YearDay)

lims <- get_limits(data_to_plot$Salinity, acc = acc_S)
mybreaks <- seq(lims[1], lims[2], by = acc_S)
mylabels <- mybreaks
mylabels[round(mylabels %% 5, 1) != 0] <- ""

p2 <- ggplot(data = data_to_plot) + 
  add_s_contour() + 
  theme(
    text = element_text(size = font_size), 
    axis.text.x = element_blank()
  ) + 
  labs(
    title = "B. Salinity", 
    fill = "PSU"
  )

#### DO contour ####
data_to_plot <- data_ctd |> 
  filter(!is.na(DO)) |> 
  group_by(Locator, Year, YearDay, BinDepth) |> 
  summarize(DO = mean(DO)) |> 
  ungroup() |> 
  rename(FakeYearDay = YearDay)

lims <- get_limits(data_to_plot$DO, acc = acc_DO)
mybreaks <- seq(lims[1], lims[2], by = acc_DO)
mylabels <- get_labels(mybreaks, even_only = TRUE)

p3 <- ggplot(data = data_to_plot) + 
  add_do_contour(hypoxia_color = FALSE) + 
  labs(
    title = "C. Dissolved oxygen", 
    fill = "mg/L"
  ) + 
  theme(
    text = element_text(size = font_size), 
    axis.text.x = element_blank()
  )

#### Chl contour ####
data_to_plot <- data_ctd |> 
  filter(!is.na(Chlorophyll)) |> 
  group_by(Locator, Year, YearDay, BinDepth) |> 
  summarize(Chlorophyll = mean(Chlorophyll)) |> 
  ungroup() |> 
  rename(FakeYearDay = YearDay)

lims <- get_limits(data_to_plot$Chlorophyll, acc = acc_chl)
mybreaks <- seq(lims[1], lims[2], by = acc_chl)
mylabels <- get_labels(mybreaks, even_only = TRUE)

p4 <- ggplot(data = data_to_plot) + 
  add_chl_contour() + 
  theme(
    text = element_text(size = font_size), 
    axis.text.x = element_blank()
  ) + 
  labs(
    fill = expression(mu*g/L), 
    title = "D. Chlorophyll fluorescence"
  )

#### N contour ####
data_to_plot <- data_ctd |> 
  filter(!is.na(NO23)) |> 
  group_by(Locator, Year, YearDay, BinDepth) |> 
  summarize(NO23 = mean(NO23)) |> 
  ungroup() |> 
  rename(FakeYearDay = YearDay)

lims <- get_limits(data_to_plot$NO23, acc = acc_N)
mybreaks <- seq(lims[1], lims[2], by = acc_N)
mylabels <- get_labels(mybreaks, even_only = TRUE)

p5 <- ggplot(data = data_to_plot) + 
  add_no23_contour() + 
  theme(
    text = element_text(size = font_size), 
    axis.text.x = element_text(size = font_size + 2, face = "bold")
  ) + 
  labs(
    fill = "mg N/L", 
    title = "E. Nitrate + nitrite"
  )

#### T mooring ####
data_to_plot <- data_buoy |>  
  filter(Temperature_final == 1)

p6 <- ggplot(data = data_to_plot, 
       aes(x = FakeDate, 
           y = Temperature, 
           color = YearGroup)) + 
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
data_to_plot <- data_buoy |> 
  filter(Salinity_final == 1)

p7 <- ggplot(
  data = data_to_plot, 
  aes(x = FakeDate, y = Salinity, color = YearGroup)
) + 
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