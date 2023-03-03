#### Setup ####
source(here::here("src", "utility_functions.R"))

data_ctd <- load_composite(0.5, monthly = FALSE)
data_discrete <- load_discrete_data()

good_quals_ctd <- c(NA, "TA")
good_quals_discrete <- 0:2

#### Figure - nitrate CTD profiles by station, month ####
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  NO23_Qual %in% good_quals_ctd), 
         aes(x = Depth, y = NO23, group = Date)) + 
    theme_bw() + 
    geom_line() + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Month) + 
    labs(x = "Depth (m)", 
         y = "Nitrate + nitrite N(mg/L)", 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "no23", 
                         paste0(station, "_NO23.png")), 
         height = 5, width = 5)
}

#### Figure - DO CTD profiles by station, month ####
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  DO_Qual %in% good_quals_ctd)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = DO, group = Date)) + 
    geom_hline(yintercept = 6, color = "orange") + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Month) + 
    labs(x = "Depth (m)", 
         y = "Dissolved oxygen (mg/L)", 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "do", 
                         paste0(station, "_DO.png")), 
         height = 5, width = 5)
}

#### Figure - chl CTD profiles by station, month ####
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  Chlorophyll_Qual %in% good_quals_ctd, 
                  Depth <= 50)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = Chlorophyll, group = Date)) + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Month) + 
    labs(x = "Depth (m)", 
         y = "Chlorophyll a (mg/L)", 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "chl", 
                         paste0(station, "_chl.png")), 
         height = 5, width = 5)
}

#### Figure - nitrate bottle by station, depth, time ####
for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_discrete %>% 
           filter(ParmId == 14, 
                  QualityId %in% good_quals_discrete, 
                  Locator == station), 
         aes(x = CollectDate, 
             y = Value, 
             shape = grepl("MDL", QfrCode))) + 
    theme_bw() + 
    geom_point() + 
    scale_shape_manual(values = c(16, 1)) + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "Nitrate (mg/L)", 
         title = station, 
         shape = "<MDL")
  ggsave(filename = here("figs", "bottle", "no23", 
                         paste0(station, "_NO23.png")), 
         height = 5, width = 5)
}

#### Figure - N and P bottle for a single deep station, year ####
yoi <- 2022
stations <- c("SARATOGACH")
for (station in stations) {
  ggplot(data = bottle_data %>% 
           filter(Locator == station, 
                  year(CollectDate) == yoi, 
                  ParmId %in% c(14, 15)), 
         aes(x = CollectDate, 
             y = Value, 
             color = Depth, 
             shape = Detect)) + 
    theme_bw() + 
    geom_point() + 
    facet_grid(rows = vars(ParmDisplayName), 
               scales = "free_y") + 
    scale_x_datetime(date_breaks = "1 month", 
                     date_labels = "%b") + 
    labs(x = "", y = "Concentration (mg/L)", 
         title = paste(station, yoi)) + 
    scale_color_gradient(trans = "log",
                         breaks = c(150, 50, 25, 10, 5, 1)) +
    scale_shape_manual(values = c(1, 16))
  ggsave(here("figs", "bottle", 
              paste0("N_P_", station, "_", yoi, ".png")), 
         dpi = 600, height = 4, width = 6)
  
}

#### Figure - phosphate bottle by station, depth, time ####
for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_discrete %>% 
           filter(ParmId == 15, 
                  QualityId %in% good_quals_discrete, 
                  Locator == station), 
         aes(x = CollectDate, 
             y = Value, 
             shape = grepl("MDL", QfrCode))) + 
    theme_bw() + 
    geom_point() + 
    scale_shape_manual(values = c(16, 1)) + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "Phosphate (mg/L)", 
         title = station, 
         shape = "<MDL")
  ggsave(filename = here("figs", "bottle", "po4", 
                         paste0(station, "_PO4.png")), 
         height = 5, width = 5)
}

#### Figure - ammonia bottle by station, depth, time ####
for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_discrete %>% 
           filter(ParmId == 13, 
                  QualityId %in% good_quals_discrete, 
                  Locator == station), 
         aes(x = CollectDate, 
             y = Value, 
             shape = grepl("MDL", QfrCode))) + 
    theme_bw() + 
    geom_point() + 
    scale_shape_manual(values = c(16, 1)) + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "Ammonia (mg/L)", 
         title = station, 
         shape = "<MDL")
  ggsave(filename = here("figs", "bottle", "nh3", 
                         paste0(station, "_NH3.png")), 
         height = 5, width = 5)
}

#### Figure - chl CTD profiles by station, year ####
yoi <- 2022
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  Chlorophyll_Qual %in% good_quals_ctd, 
                  Depth <= 50, 
                  year(Date) == yoi)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = Chlorophyll)) + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Date) + 
    labs(x = "Depth (m)", 
         y = expression(Chlorophyll~(mu*g/L)), 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "chl", 
                         paste0(station, "_", yoi, "_chl.png")), 
         height = 5, width = 5)
}

#### Figure - surface density CTD profiles by station, year ####
yoi <- 2022
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  SigmaTheta_Qual %in% good_quals_ctd, 
                  Depth <= 50, 
                  year(Date) == yoi)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = SigmaTheta)) + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Date) + 
    labs(x = "Depth (m)", 
         y = expression(sigma [theta]~(kg/m^3)), 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "density", 
                         paste0(station, "_", yoi, "_surface_sigmat.png")), 
         height = 5, width = 5)
}

