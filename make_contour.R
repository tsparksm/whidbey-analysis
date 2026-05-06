#### Load libraries and functions ####
source(here::here("src", "utility_functions.R"))
source(here::here("src", "contour_functions.R"))

#### Load composite data ####
# How wide do you want your depth bins (0.5, 1, 2, 5 probably best)
bin_width <- 0.5

station_order <- c(
  "SARATOGARP",
  "SARATOGAOP",
  "SARATOGACH",
  "PENNCOVEENT",
  "PENNCOVECW",
  "PENNCOVEWEST",
  "PSUSANBUOY",
  "PSUSANKP",
  "PSUSANENT",
  "Poss DO-2"
)

data_ctd <- load_composite(bin_width, monthly = FALSE) |> 
  mutate(
    YearDay = yday(Date),
    Locator = factor(Locator, levels = station_order)
  )

#### SETUP ####
# Select stations, years; figure settings
stations <- dlg_list(station_order, multiple = TRUE)$res

min_year <- as.numeric(dlg_input("Minimum year? YYYY")$res)
max_year <- as.numeric(dlg_input("Maximum year? YYYY")$res)
years_to_plot <- min_year:max_year
n_years <- length(years_to_plot)

# Mostly unchanging figure parameters
# Do you want contour lines on your sigma-theta plots?
sigmat_contour_alpha <- 0.1  # if you want contour lines, use 0.1; else use 0

# How narrowly spaced do you want the color bins in each of these plots?
# Original values are in comments following semi-colon
acc_DO_anom <- 0.05  # DO anomaly; 0.05
acc_DO <- 0.2  # DO; 0.2
acc_S_anom <- 0.025  # salinity anomaly; 0.025
acc_S <- 0.1  # salinity; 0.1
acc_T_anom <- 0.05  # temperature anomaly; 0.05
acc_T <- 0.2  # temperature; 0.2
acc_sigmaT <- 0.2  # sigma-theta density; 0.2
acc_NO23 <- 0.01

# Set figure sizes
h <- 2
w <- 7


#### Calculate max_depth for each station ####
max_depth <- data_ctd %>% 
  filter(Year %in% years_to_plot) %>% 
  group_by(Locator, Year, YearDay) %>% 
  summarize(MaxDepth = max(BinDepth, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Locator) %>% 
  summarize(MinMaxDepth = min(MaxDepth))

#### Add extra data before and after each year ####
data_remix <- data_ctd %>% 
  filter(Locator %in% stations) %>% 
  mutate(FakeYearDay = YearDay)

for (station in stations) {
  for (yoi in years_to_plot) {
    if (min(data_remix$Year) < yoi) {
      extra_data_before <- data_remix %>% 
        filter(Year == yoi - 1, 
               Locator == station) %>% 
        filter(YearDay == max(YearDay)) %>% 
        mutate(FakeYearDay = YearDay - 365, 
               Year = yoi)
      data_remix <- add_row(data_remix, extra_data_before)
    }

    if (max(data_remix$Year) > yoi) {
      extra_data_after <- data_remix %>% 
        filter(Year == yoi + 1, 
               Locator == station) %>% 
        filter(YearDay == min(YearDay)) %>% 
        mutate(FakeYearDay = YearDay + 365, 
               Year = yoi)
      data_remix <- add_row(data_remix, extra_data_after)
    }
  }
}

data_remix <- data_remix %>% 
  filter(Year %in% years_to_plot) %>% 
  left_join(max_depth)

#### Sigma-t contour plot ####
lims <- c(20, 23.6)  # min and max sigma-t values
mybreaks <- seq(lims[1], lims[2], by = acc_sigmaT)
mylabels <- get_labels(mybreaks, min_lim = lims[1], max_lim = lims[2])
plot_type <- "sigmat"

data_to_plot <- data_remix |> 
  filter(
    Locator %in% stations, 
    !is.na(SigmaTheta), 
    BinDepth <= MinMaxDepth
  ) |> 
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(SigmaTheta = mean(SigmaTheta, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(
    SigmaTheta = case_when(
      SigmaTheta < lims[1] ~ lims[1] + 1e-6, 
      SigmaTheta > lims[2] ~ lims[2] - 1e-6, 
      TRUE ~ SigmaTheta
    )
  ) |> 
  arrange(desc(Year))

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_sigmat_contour()
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = yoi) + 
    add_sigmat_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = station) + 
    add_sigmat_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi)) + 
      add_sigmat_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}

#### Surface sigma-t contour plot ####
surface_depth <- 40
lims <- c(20, 23.6)  # min and max sigma-t values
mybreaks <- seq(lims[1], lims[2], by = acc_sigmaT)
mylabels <- get_labels(mybreaks, min_lim = lims[1], max_lim = lims[2])
plot_type <- "sigmat_surface"

data_to_plot <- data_remix |>  
  filter(
    Locator %in% stations, 
    !is.na(SigmaTheta), 
    BinDepth <= surface_depth
  ) |>  
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(SigmaTheta = mean(SigmaTheta, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(
    SigmaTheta = case_when(
      SigmaTheta < lims[1] ~ lims[1] + 1e-6, 
      SigmaTheta > lims[2] ~ lims[2] - 1e-6, 
      TRUE ~ SigmaTheta
    )
  ) |> 
  arrange(desc(Year))

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_sigmat_contour() + 
  labs(title = paste0("Surface 0-", surface_depth, " m"))
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = paste(yoi, "surface 0-", surface_depth, " m")) + 
    add_sigmat_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = paste(station, "surface 0-", surface_depth, " m")) + 
    add_sigmat_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi, "surface 0-", surface_depth, " m")) + 
      add_sigmat_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}

#### DO contour plot ####
plot_type <- "DO"
data_to_plot <- data_remix |> 
  filter(
    Locator %in% stations, 
    !is.na(DO), 
    BinDepth <= MinMaxDepth
  ) |> 
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(DO = mean(DO, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(Year))

# Calculate whole dataset limits - will be overwritten as needed later
lims <- get_limits(data_to_plot$DO, acc_DO)
mybreaks <- seq(lims[1], lims[2], by = acc_DO)
mylabels <- get_labels(mybreaks, even_only = TRUE)

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_do_contour()
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = yoi) + 
    add_do_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = station) + 
    add_do_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi)) + 
      add_do_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}

#### Temperature contour plot ####
plot_type <- "T"
data_to_plot <- data_remix |> 
  filter(
    Locator %in% stations, 
    !is.na(Temperature), 
    BinDepth <= MinMaxDepth
  ) |> 
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(Temperature = mean(Temperature, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(Year))

# Calculate whole dataset limits - will be overwritten as needed later
lims <- get_limits(data_to_plot$Temperature, acc_T)
mybreaks <- seq(lims[1], lims[2], by = acc_T)
mylabels <- get_labels(mybreaks)

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_t_contour()
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = yoi) + 
    add_t_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = station) + 
    add_t_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi)) + 
      add_t_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}

#### Salinity contour plot ####
min_lim <- 16  # works best with even value; set as NA if you don't want cutoff

plot_type <- "S"
data_to_plot <- data_remix |> 
  filter(
    Locator %in% stations, 
    !is.na(Salinity), 
    BinDepth <= MinMaxDepth
  ) |>
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(Salinity = mean(Salinity, na.rm = TRUE)) |> 
  ungroup() |>
  mutate(Salinity = ifelse(Salinity < lims[1], lims[1] + 1e-6, Salinity)) |> 
  arrange(desc(Year))

# Calculate whole dataset limits - will be overwritten as needed later
lims <- get_limits(data_to_plot$Salinity, acc_S)
if (!is.na(min_lim)) {
  lims[1] <- min_lim
  mybreaks <- seq(lims[1], lims[2], by = acc_S)
  mylabels <- get_labels(mybreaks, min_lim = min_lim, even_only = TRUE)
} else {
  mybreaks <- seq(lims[1], lims[2], by = acc_S)
  mylabels <- get_labels(mybreaks, even_only = TRUE)
}

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_s_contour()
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = yoi) + 
    add_s_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = station) + 
    add_s_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi)) + 
      add_s_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}

#### Surface salinity contour plot ####
min_lim <- 16  # works best with even value; set as NA if you don't want cutoff
surface_depth <- 40

plot_type <- "S_surface"
data_to_plot <- data_remix |> 
  filter(
    Locator %in% stations, 
    !is.na(Salinity), 
    BinDepth <= surface_depth
  ) |>
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(Salinity = mean(Salinity, na.rm = TRUE)) |> 
  ungroup() |>
  mutate(Salinity = ifelse(Salinity < lims[1], lims[1] + 1e-2, Salinity)) |> 
  arrange(desc(Year))

# Calculate whole dataset limits - will be overwritten as needed later
lims <- get_limits(data_to_plot$Salinity, acc_S)
if (!is.na(min_lim)) {
  lims[1] <- min_lim
  mybreaks <- seq(lims[1], lims[2], by = acc_S)
  mylabels <- get_labels(mybreaks, min_lim = min_lim, even_only = TRUE)
} else {
  mybreaks <- seq(lims[1], lims[2], by = acc_S)
  mylabels <- get_labels(mybreaks, even_only = TRUE)
}

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_s_contour()
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = yoi) + 
    add_s_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = station) + 
    add_s_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi)) + 
      add_s_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}

#### NO23 contour plot ####
plot_type <- "NO23"
data_to_plot <- data_remix |> 
  filter(
    Locator %in% stations, 
    !is.na(NO23), 
    BinDepth <= MinMaxDepth
  ) |>
  group_by(Locator, Year, FakeYearDay, BinDepth) |> 
  summarize(NO23 = mean(NO23, na.rm = TRUE)) |> 
  ungroup() |>
  arrange(desc(Year))

# Calculate whole dataset limits - will be overwritten as needed later
lims <- get_limits(data_to_plot$NO23, acc_NO23)
mybreaks <- seq(lims[1], lims[2], by = acc_NO23)
mylabels <- get_labels(mybreaks, round = FALSE)

# All stations + years on one figure
png(
  get_filename(plot_type, stations, years_to_plot), 
  height = h*length(stations), 
  width = w*n_years, 
  units = "in", 
  res = 600
)
ggplot(data = data_to_plot |> filter(Year %in% years_to_plot)) + 
  facet_grid(rows = vars(Locator), cols = vars(Year), scales = "free_y") + 
  add_no23_contour()
dev.off()

# All stations on one figure; one figure for each year
for (yoi in years_to_plot) {
  png(
    get_filename(plot_type, stations, yoi), 
    height = h*length(stations), 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(data = data_to_plot |> filter(Year == yoi)) + 
    facet_wrap(~ Locator, ncol = 1, scales = "free_y") + 
    labs(title = yoi) + 
    add_s_contour()
  print(p)
  dev.off()
}

# All years on one figure; one figure for each station
for (station in stations) {
  png(
    get_filename(plot_type, station, years_to_plot), 
    height = h*n_years, 
    width = w, 
    units = "in", 
    res = 600
  )
  p <- ggplot(
    data = data_to_plot |> 
      filter(Year %in% years_to_plot, Locator == station)
  ) + 
    facet_wrap(
      ~factor(Year, levels = rev(years_to_plot)), 
      ncol = 1, 
      scales = "free_y"
    ) + 
    labs(title = station) + 
    add_s_contour()
  print(p)
  dev.off()
}

# One figure for each station + year
for (station in stations) {
  for (yoi in years_to_plot) {
    png(
      get_filename(plot_type, station, yoi), 
      height = h, 
      width = w, 
      units = "in", 
      res = 600
    )
    p <- ggplot(data = data_to_plot |> filter(Year == yoi, Locator == station)) + 
      labs(title = paste(station, yoi)) + 
      add_s_contour() + 
      theme(
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = 0.5
        )
      )
    print(p)
    dev.off()
  }
}