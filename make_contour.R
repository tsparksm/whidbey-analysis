#### Setup ####
source(here::here("src", "utility_functions.R"))
source(here::here("src", "contour_functions.R"))
library(metR)
library(cmocean)
library(scales)

# What stations to plot?
# stations <- c("SARATOGACH", "SARATOGAOP", "SARATOGARP")
# stations <- c("PSUSANENT", "PSUSANKP", "PSUSANBUOY")
# stations <- c("PENNCOVECW", "PENNCOVEENT", "PENNCOVEWEST")
# stations <- c("SARATOGACH", "SARATOGAOP", "SARATOGARP",
#               "Poss DO-2", "PSUSANENT", "PSUSANKP", "PSUSANBUOY",
#               "PENNCOVECW", "PENNCOVEENT", "PENNCOVEWEST")
# stations <- c("Poss DO-2", "PSUSANENT", "PSUSANKP", 
#               "SARATOGACH", "SARATOGAOP", "SARATOGARP")
stations <- "PSUSANBUOY"

# All stations on one figure? TRUE or FALSE
all_stations_fig <- FALSE

# If all_stations_fig == FALSE, same limits across stations? TRUE or FALSE
all_stations_lims <- FALSE

# What years to plot? c(min_year, max_year)
years <- c(2022, 2022)
n <- length(unique(years))

# All years on one figure? TRUE or FALSE
all_years_fig <- TRUE

# If all_years_fig == FALSE, same limits across years? TRUE or FALSE
all_years_lims <- TRUE

# Do you want contour lines on your sigma-theta plots?
sigmat_contour_alpha <- 0.1  # if you want contour lines, use 0.1; else use 0

# How wide do you want your depth bins (0.5, 1, 2, 5 probably best)
bin_width <- 0.5

# Load composite data
data_ctd <- load_composite(bin_width, 
                           monthly = FALSE) %>% 
  mutate(YearDay = yday(Date))

# How narrowly spaced do you want the color bins in each of these plots?
# Original values are in comments following semi-colon
acc_DO_anom <- 0.05  # DO anomaly; 0.05
acc_DO <- 0.2  # DO; 0.2
acc_S_anom <- 0.025  # salinity anomaly; 0.025
acc_S <- 0.1  # salinity; 0.1
acc_T_anom <- 0.05  # temperature anomaly; 0.05
acc_T <- 0.2  # temperature; 0.2
acc_sigmaT <- 0.2  # sigma-theta density; 0.2
acc_NO23 <- 0.05

#### Calculate max_depth for each station ####
max_depth <- data_ctd %>% 
  filter(Year <= years[2], 
         Year >= years[1]) %>% 
  group_by(Locator, Year, YearDay) %>% 
  summarize(MaxDepth = max(BinDepth, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Locator) %>% 
  summarize(MinMaxDepth = min(MaxDepth))

#### Add extra data before and after each year; filter by MinMaxDepth ####
data_remix <- data_ctd %>% 
  filter(Locator %in% stations) %>% 
  mutate(FakeYearDay = YearDay)

for (station in stations) {
  for (yoi in years[1]:years[2]) {
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
  filter(Year %in% years[1]:years[2]) %>% 
  left_join(max_depth)

#### Sigma-t contour plot ####
lims <- c(20, 23.6)  # min and max sigma-t values
mybreaks <- seq(lims[1], lims[2], by = acc_sigmaT)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
mylabels[1] <- paste0("<", lims[1])
mylabels[length(mylabels)] <- paste0(">", lims[2])

data_to_plot <- data_remix %>% 
  filter(Locator %in% stations, 
         !is.na(SigmaTheta), 
         BinDepth <= MinMaxDepth) %>% 
  group_by(Locator, Year, FakeYearDay, BinDepth) %>% 
  summarize(SigmaTheta = mean(SigmaTheta, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SigmaTheta = case_when(SigmaTheta < lims[1] ~ lims[1], 
                                SigmaTheta > lims[2] ~ lims[2], 
                                TRUE ~ SigmaTheta))

if (all_stations_fig) {
  if (all_years_fig) {
    ggplot(data = data_to_plot %>% 
             filter(Year %in% years[1]:years[2])) + 
      theme_classic() + 
      facet_grid(rows = vars(Locator), 
                 cols = vars(Year), 
                 scales = "free_y") + 
      metR::geom_contour_fill(aes(x = FakeYearDay, 
                                  y = BinDepth, 
                                  z = SigmaTheta), 
                              na.fill = TRUE, 
                              breaks = mybreaks, 
                              color = alpha("white", sigmat_contour_alpha)) + 
      scale_fill_cmocean(name = "dense", 
                         breaks = mybreaks, 
                         limits = lims, 
                         labels = mylabels, 
                         guide = guide_colorbar(show.limits = TRUE, 
                                                ticks = FALSE, 
                                                reverse = TRUE)) + 
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
      geom_vline(aes(xintercept = FakeYearDay), 
                 alpha = 0.2) + 
      labs(x = "", 
           y = "Depth (m)", 
           fill = expression(kg/m^3), 
           title = bquote(sigma[Theta]))
    ggsave(here("figs", "contour", "sigmat", 
                paste0(paste(stations, collapse = "_"), 
                       "_sigmat_", 
                       years[1], "-", years[2], 
                       ".png")), 
           height = 1.5*length(stations), 
           width = 5*n, 
           dpi = 600)
  } else {
    for (yoi in years[1]:years[2]) {
      ggplot(data = data_to_plot %>% 
               filter(Year %in% yoi)) + 
        theme_classic() + 
        facet_wrap(~ Locator, 
                   ncol = 1, 
                   scales = "free_y") + 
        metR::geom_contour_fill(aes(x = FakeYearDay, 
                                    y = BinDepth, 
                                    z = SigmaTheta), 
                                na.fill = TRUE, 
                                breaks = mybreaks, 
                                color = alpha("white", sigmat_contour_alpha)) + 
        scale_fill_cmocean(name = "dense", 
                           breaks = mybreaks, 
                           limits = lims, 
                           labels = mylabels, 
                           guide = guide_colorbar(show.limits = TRUE, 
                                                  ticks = FALSE, 
                                                  reverse = TRUE)) + 
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
        geom_vline(aes(xintercept = FakeYearDay), 
                   alpha = 0.2) + 
        labs(x = "", 
             y = "Depth (m)", 
             fill = expression(kg/m^3), 
             title = bquote(sigma[Theta]~yoi))
      ggsave(here("figs", "contour", "sigmat", 
                  paste0(paste(stations, collapse = "_"), 
                         "_sigmat_", 
                         yoi, 
                         ".png")), 
             height = 1.5*length(stations), 
             width = 5, 
             dpi = 600)
    }
  }
} else { 
  for (station in stations) {
    if (all_years_fig) {
      ggplot(data = data_to_plot %>% 
               filter(Year %in% years[1]:years[2], 
                      Locator == station)) + 
        theme_classic() + 
        facet_wrap(~ Year, 
                   ncol = 1, 
                   scales = "free_y") + 
        metR::geom_contour_fill(aes(x = FakeYearDay, 
                                    y = BinDepth, 
                                    z = SigmaTheta), 
                                na.fill = TRUE, 
                                breaks = mybreaks, 
                                color = alpha("white", sigmat_contour_alpha)) + 
        scale_fill_cmocean(name = "dense", 
                           breaks = mybreaks, 
                           limits = lims, 
                           labels = mylabels, 
                           guide = guide_colorbar(show.limits = TRUE, 
                                                  ticks = FALSE, 
                                                  reverse = TRUE)) + 
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
        geom_vline(aes(xintercept = FakeYearDay), 
                   alpha = 0.2) + 
        labs(x = "", 
             y = "Depth (m)", 
             fill = expression(kg/m^3), 
             title = bquote(.(station)~sigma[Theta]))
      ggsave(here("figs", "contour", "sigmat", station, 
                  paste0(station, "_sigmat_", 
                         years[1], "-", years[2], 
                         ".png")), 
             height = 1.5*n, 
             width = 5, 
             dpi = 600)
    } else {
      for (yoi in years[1]:years[2]) {
        ggplot(data = data_to_plot %>% 
                 filter(Year == yoi, 
                        Locator == station)) + 
          theme_classic() + 
          theme(panel.border = element_rect(colour = "black", 
                                            fill = NA, 
                                            linewidth = 0.5)) + 
          metR::geom_contour_fill(aes(x = FakeYearDay, 
                                      y = BinDepth, 
                                      z = SigmaTheta), 
                                  na.fill = TRUE, 
                                  breaks = mybreaks, 
                                  color = alpha("white", sigmat_contour_alpha)) + 
          scale_fill_cmocean(name = "dense", 
                             breaks = mybreaks, 
                             limits = lims, 
                             labels = mylabels, 
                             guide = guide_colorbar(show.limits = TRUE, 
                                                    ticks = FALSE, 
                                                    reverse = TRUE)) + 
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
          geom_vline(aes(xintercept = FakeYearDay), 
                     alpha = 0.2) + 
          labs(x = "", 
               y = "Depth (m)", 
               fill = expression(kg/m^3), 
               title = bquote(.(station)~sigma[Theta]~yoi))
        ggsave(here("figs", "contour", "sigmat", station, 
                    paste0(station, "_sigmat_", 
                           yoi, 
                           ".png")), 
               height = 2, 
               width = 5, 
               dpi = 600)
      }
    }
  }
}

#### Surface sigma-t contour plot ####
surface_depth <- 40
lims <- c(20, 23.6)  # min and max sigma-t values
mybreaks <- seq(lims[1], lims[2], by = acc_sigmaT)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
mylabels[1] <- paste0("<", lims[1])
mylabels[length(mylabels)] <- paste0(">", lims[2])

data_to_plot <- data_remix %>% 
  filter(Locator %in% stations, 
         !is.na(SigmaTheta), 
         BinDepth <= surface_depth) %>% 
  group_by(Locator, Year, FakeYearDay, BinDepth) %>% 
  summarize(SigmaTheta = mean(SigmaTheta, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(SigmaTheta = case_when(SigmaTheta < lims[1] ~ lims[1], 
                                SigmaTheta > lims[2] ~ lims[2], 
                                TRUE ~ SigmaTheta))

if (all_stations_fig) {
  if (all_years_fig) {
    ggplot(data = data_to_plot %>% 
             filter(Year %in% years[1]:years[2])) + 
      theme_classic() + 
      facet_grid(rows = vars(Locator), 
                 cols = vars(Year), 
                 scales = "free_y") + 
      metR::geom_contour_fill(aes(x = FakeYearDay, 
                                  y = BinDepth, 
                                  z = SigmaTheta), 
                              na.fill = TRUE, 
                              breaks = mybreaks, 
                              color = alpha("white", sigmat_contour_alpha)) + 
      scale_fill_cmocean(name = "dense", 
                         breaks = mybreaks, 
                         limits = lims, 
                         labels = mylabels, 
                         guide = guide_colorbar(show.limits = TRUE, 
                                                ticks = FALSE, 
                                                reverse = TRUE)) + 
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
      geom_vline(aes(xintercept = FakeYearDay), 
                 alpha = 0.2) + 
      labs(x = "", 
           y = "Depth (m)", 
           fill = expression(kg/m^3), 
           title = bquote(sigma[Theta]~surface))
    ggsave(here("figs", "contour", "sigmat", 
                paste0(paste(stations, collapse = "_"), 
                       "_surface_sigmat_", 
                       years[1], "-", years[2], 
                       ".png")), 
           height = 1.5*length(stations), 
           width = 5*n, 
           dpi = 600)
  } else {
    for (yoi in years[1]:years[2]) {
      ggplot(data = data_to_plot %>% 
               filter(Year %in% yoi)) + 
        theme_classic() + 
        facet_wrap(~ Locator, 
                   ncol = 1, 
                   scales = "free_y") + 
        metR::geom_contour_fill(aes(x = FakeYearDay, 
                                    y = BinDepth, 
                                    z = SigmaTheta), 
                                na.fill = TRUE, 
                                breaks = mybreaks, 
                                color = alpha("white", sigmat_contour_alpha)) + 
        scale_fill_cmocean(name = "dense", 
                           breaks = mybreaks, 
                           limits = lims, 
                           labels = mylabels, 
                           guide = guide_colorbar(show.limits = TRUE, 
                                                  ticks = FALSE, 
                                                  reverse = TRUE)) + 
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
        geom_vline(aes(xintercept = FakeYearDay), 
                   alpha = 0.2) + 
        labs(x = "", 
             y = "Depth (m)", 
             fill = expression(kg/m^3), 
             title = bquote(sigma[Theta]~yoi~surface))
      ggsave(here("figs", "contour", "sigmat", 
                  paste0(paste(stations, collapse = "_"), 
                         "_surface_sigmat_", 
                         yoi, 
                         ".png")), 
             height = 1.5*length(stations), 
             width = 5, 
             dpi = 600)
    }
  }
} else { 
  for (station in stations) {
    if (all_years_fig) {
      ggplot(data = data_to_plot %>% 
               filter(Year %in% years[1]:years[2], 
                      Locator == station)) + 
        theme_classic() + 
        facet_wrap(~ Year, 
                   ncol = 1, 
                   scales = "free_y") + 
        metR::geom_contour_fill(aes(x = FakeYearDay, 
                                    y = BinDepth, 
                                    z = SigmaTheta), 
                                na.fill = TRUE, 
                                breaks = mybreaks, 
                                color = alpha("white", sigmat_contour_alpha)) + 
        scale_fill_cmocean(name = "dense", 
                           breaks = mybreaks, 
                           limits = lims, 
                           labels = mylabels, 
                           guide = guide_colorbar(show.limits = TRUE, 
                                                  ticks = FALSE, 
                                                  reverse = TRUE)) + 
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
        geom_vline(aes(xintercept = FakeYearDay), 
                   alpha = 0.2) + 
        labs(x = "", 
             y = "Depth (m)", 
             fill = expression(kg/m^3), 
             title = bquote(.(station)~sigma[Theta]~surface))
      ggsave(here("figs", "contour", "sigmat", station, 
                  paste0(station, "_surface_sigmat_", 
                         years[1], "-", years[2], 
                         ".png")), 
             height = 1.5*n, 
             width = 5, 
             dpi = 600)
    } else {
      for (yoi in years[1]:years[2]) {
        ggplot(data = data_to_plot %>% 
                 filter(Year == yoi, 
                        Locator == station)) + 
          theme_classic() + 
          theme(panel.border = element_rect(colour = "black", 
                                            fill = NA, 
                                            linewidth = 0.5)) + 
          metR::geom_contour_fill(aes(x = FakeYearDay, 
                                      y = BinDepth, 
                                      z = SigmaTheta), 
                                  na.fill = TRUE, 
                                  breaks = mybreaks, 
                                  color = alpha("white", sigmat_contour_alpha)) + 
          scale_fill_cmocean(name = "dense", 
                             breaks = mybreaks, 
                             limits = lims, 
                             labels = mylabels, 
                             guide = guide_colorbar(show.limits = TRUE, 
                                                    ticks = FALSE, 
                                                    reverse = TRUE)) + 
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
          geom_vline(aes(xintercept = FakeYearDay), 
                     alpha = 0.2) + 
          labs(x = "", 
               y = "Depth (m)", 
               fill = expression(kg/m^3), 
               title = bquote(.(station)~sigma[Theta]~yoi~surface))
        ggsave(here("figs", "contour", "sigmat", station, 
                    paste0(station, "_surface_sigmat_", 
                           yoi, 
                           ".png")), 
               height = 2, 
               width = 5, 
               dpi = 600)
      }
    }
  }
}

#### DO contour plot ####
data_to_plot <- data_remix %>% 
  filter(Locator %in% stations, 
         !is.na(DO), 
         BinDepth <= MinMaxDepth) %>% 
  group_by(Locator, Year, FakeYearDay, BinDepth) %>% 
  summarize(DO = mean(DO, na.rm = TRUE)) %>% 
  ungroup()

# Calculate whole dataset limits - will be overwritten as needed later
min_lim <- round_any(min(data_to_plot$DO, na.rm = T),
                     accuracy = acc_DO, f = floor)
max_lim <- round_any(max(data_to_plot$DO, na.rm = T),
                     accuracy = acc_DO, f = ceiling)
mybreaks <- seq(min_lim, max_lim, by = acc_DO)
mylabels <- mybreaks
mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""

if (all_stations_fig) {
  if (all_years_fig) {
    
    p <- contour_do(data_to_plot, mybreaks, mylabels, min_lim, max_lim) + 
      facet_grid(rows = vars(Locator), 
                 cols = vars(Year), 
                 scales = "free_y")
    
    ggsave(here("figs", "contour", "DO", 
                paste0(paste(stations, collapse = "_"), 
                       "_DO_", 
                       years[1], "-", years[2], 
                       ".png")), 
           p, 
           height = 1.5*length(stations), 
           width = 5*n, 
           dpi = 600)
    
  } else {
    for (yoi in years[1]:years[2]) {
      temp <- data_to_plot %>% 
        filter(Year == yoi)
      
      if (!all_years_lims) {
        min_lim <- round_any(min(temp$DO, na.rm = T),
                             accuracy = acc_DO, f = floor)
        max_lim <- round_any(max(temp$DO, na.rm = T),
                             accuracy = acc_DO, f = ceiling)
        mybreaks <- seq(min_lim, max_lim, by = acc_DO)
        mylabels <- mybreaks
        mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""
      }
      
      p <- contour_do(temp, mybreaks, mylabels, min_lim, max_lim) + 
        facet_wrap(~ Locator, 
                   ncol = 1, 
                   scales = "free_y") + 
        labs(title = paste("DO", yoi))
          
      ggsave(here("figs", "contour", "DO", 
                  paste0(paste(stations, collapse = "_"), 
                         "_DO_", 
                         yoi, 
                         ".png")), 
             p, 
             height = 1.5*length(stations), 
             width = 5, 
             dpi = 600)
    }
  }
} else {
  for (station in stations) {
    
    temp <- data_to_plot %>% filter(Locator == station)
    
    if (all_years_fig) {
      if (!all_stations_lims) {
        min_lim <- round_any(min(temp$DO, na.rm = T),
                             accuracy = acc_DO, f = floor)
        max_lim <- round_any(max(temp$DO, na.rm = T) + acc_DO,
                             accuracy = acc_DO, f = ceiling)
        mybreaks <- seq(min_lim, max_lim, by = acc_DO)
        mylabels <- mybreaks
        mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""
      }
      
      p <- contour_do(temp, mybreaks, mylabels, min_lim, max_lim) + 
        facet_wrap(~ Year, 
                   ncol = 1, 
                   scales = "free_y") + 
        labs(title = station)
      
      ggsave(here("figs", "contour", "DO", station, 
                  paste0(station, 
                         "_DO_", 
                         years[1], "-", years[2], 
                         ".png")), 
             p, 
             height = 1.5*n, 
             width = 5, 
             dpi = 600)
      
    } else {
      
      for (yoi in years[1]:years[2]) {
        
        temp <- data_to_plot %>% filter(Locator == station, 
                                        Year == yoi)
        
        if (!all_stations_lims & !all_years_lims) {
          min_lim <- round_any(min(temp$DO, na.rm = T),
                               accuracy = acc_DO, f = floor)
          max_lim <- round_any(max(temp$DO, na.rm = T) + acc_DO,
                               accuracy = acc_DO, f = ceiling)
          mybreaks <- seq(min_lim, max_lim, by = acc_DO)
          mylabels <- mybreaks
          mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""
        }
        if (all_stations_lims & !all_years_lims) {
          temp_lim <- data_to_plot %>% filter(Year == yoi)
          min_lim <- round_any(min(temp_lim$DO, na.rm = T),
                               accuracy = acc_DO, f = floor)
          max_lim <- round_any(max(temp_lim$DO, na.rm = T),
                               accuracy = acc_DO, f = ceiling)
          mybreaks <- seq(min_lim, max_lim, by = acc_DO)
          mylabels <- mybreaks
          mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""
        } 
        if (!all_stations_lims & all_years_lims) {
          temp_lim <- data_to_plot %>% filter(Locator == station)
          min_lim <- round_any(min(temp_lim$DO, na.rm = T),
                               accuracy = acc_DO, f = floor)
          max_lim <- round_any(max(temp_lim$DO, na.rm = T) + acc_DO,
                               accuracy = acc_DO, f = ceiling)
          mybreaks <- seq(min_lim, max_lim, by = acc_DO)
          mylabels <- mybreaks
          mylabels[!(round(mylabels/2, 2) == round(round(mylabels/2, 2)))] <- ""
        }
        
        p <- contour_do(temp, mybreaks, mylabels, min_lim, max_lim) + 
          labs(title = paste(station, yoi))
        
        ggsave(here("figs", "contour", "DO", station, 
                    paste0(station, 
                           "_DO_", 
                           yoi, 
                           ".png")), 
               p, 
               height = 2, 
               width = 7, 
               dpi = 600)
      }
    }
  }
}

#### Temperature contour plot ####
data_to_plot <- data_remix %>% 
  filter(Locator %in% stations, 
         !is.na(Temperature), 
         BinDepth <= MinMaxDepth) %>% 
  group_by(Locator, Year, FakeYearDay, BinDepth) %>% 
  summarize(Temperature = mean(Temperature, na.rm = TRUE)) %>% 
  ungroup()

if (all_stations_fig) {
  
  min_lim <- round_any(min(data_to_plot$Temperature, na.rm = T),
                       accuracy = acc_T, f = floor)
  max_lim <- round_any(max(data_to_plot$Temperature, na.rm = T),
                       accuracy = acc_T, f = ceiling)
  mybreaks <- seq(min_lim, max_lim, by = acc_T)
  mylabels <- mybreaks
  mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    facet_wrap(~ Locator, 
               ncol = 1, 
               scales = "free_y") + 
    metR::geom_contour_fill(aes(x = FakeYearDay, 
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
    geom_vline(aes(xintercept = FakeYearDay), 
               alpha = 0.2) + 
    labs(x = "", 
         y = "Depth (m)", 
         fill = expression( degree*C), 
         title = ("Temperature"))
  ggsave(here("figs", "contour", "T", 
              paste0(paste(stations, collapse = "_"), 
                     "_T_", 
                     years[1], "_", years[2], 
                     ".png")), 
         height = 2*length(stations), 
         width = 8, 
         dpi = 600)
} else {
  for (station in stations) {
    temp <- data_to_plot %>% filter(Locator == station)
    
    min_lim <- round_any(min(temp$Temperature, na.rm = T),
                         accuracy = acc_T, f = floor)
    max_lim <- round_any(max(temp$Temperature, na.rm = T),
                         accuracy = acc_T, f = ceiling)
    mybreaks <- seq(min_lim, max_lim, by = acc_T)
    mylabels <- mybreaks
    mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
    
    ggplot(data = temp) + 
      theme_classic() + 
      facet_wrap(~ Year, 
                 ncol = 1) + 
      metR::geom_contour_fill(aes(x = FakeYearDay, 
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
      geom_vline(aes(xintercept = FakeYearDay), 
                 alpha = 0.2) + 
      labs(x = "", 
           y = "Depth (m)", 
           fill = expression( degree*C), 
           title = paste(station, "temperature"))
    ggsave(here("figs", "contour", "T", station, 
                paste0(station, "_T_", 
                       years[1], "_", years[2], 
                       ".png")), 
           height = 2*n, 
           width = 8, 
           dpi = 600)
  }
}
  
#### Salinity contour plot - surface ####
max_depth <- 20
if (exists("min_lim")) rm(min_lim)
# min_lim <- 15

data_to_plot <- data_remix %>% 
  filter(Locator %in% stations, 
         !is.na(Salinity), 
         BinDepth <= max_depth) %>% 
  group_by(Locator, Year, FakeYearDay, BinDepth) %>% 
  summarize(Salinity = mean(Salinity, na.rm = TRUE)) %>% 
  ungroup()

if (all_stations_fig) {
  if (exists("min_lim")) {
    data_to_plot <- data_to_plot %>% 
      mutate(Salinity = ifelse(Salinity < min_lim, min_lim, Salinity))
  } else {
    min_lim <- round_any(min(data_to_plot$Salinity, na.rm = TRUE), 
                         accurac = acc_S, f = floor)
    set_min <- FALSE
  }
  
  max_lim <- round_any(max(data_to_plot$Salinity, na.rm = TRUE), 
                       accuracy = acc_S, f = ceiling)
  mybreaks <- seq(min_lim, max_lim, by = acc_S)
  mylabels <- mybreaks
  mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
  if (set_min) {
    mylabels[1] <- paste0("<", min_lim)
  }
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    facet_wrap(~ Locator, 
               ncol = 1, 
               scales = "free_y") + 
    metR::geom_contour_fill(aes(x = FakeYearDay, 
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
    geom_vline(aes(xintercept = FakeYearDay), 
               alpha = 0.2) + 
    labs(x = "", 
         y = "Depth (m)", 
         fill = "PSU", 
         title = "Surface salinity")
  ggsave(here("figs", "contour", "S", 
              paste0(paste(stations, collapse = "_"), 
                     "_surface_S_", 
                     years[1], "_", years[2], 
                     ".png")), 
         height = 2*length(stations), 
         width = 8, 
         dpi = 600)
} else {
  for (station in stations) {
    temp <- data_to_plot %>% 
      filter(Locator == station)
    
    if (exists("min_lim")) {
      temp <- data_to_plot %>% 
        mutate(Salinity = ifelse(Salinity < min_lim, min_lim, Salinity))
    } else {
      min_lim <- round_any(min(temp$Salinity, na.rm = TRUE), 
                           accurac = acc_S, f = floor)
      set_min <- FALSE
    }
    
    max_lim <- round_any(max(temp$Salinity, na.rm = TRUE), 
                         accuracy = acc_S, f = ceiling)
    mybreaks <- seq(min_lim, max_lim, by = acc_S)
    mylabels <- mybreaks
    mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
    if (set_min) {
      mylabels[1] <- paste0("<", min_lim)
    }
      
    ggplot(data = temp) + 
      theme_classic() + 
      facet_wrap(~ Year, 
                 ncol = 1) + 
      metR::geom_contour_fill(aes(x = FakeYearDay, 
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
      geom_vline(aes(xintercept = FakeYearDay), 
                 alpha = 0.2) + 
      labs(x = "", 
           y = "Depth (m)", 
           fill = "PSU", 
           title = paste(station, "surface salinity"))
    ggsave(here("figs", "contour", "S", station, 
                paste0(station, "_surface_S_", 
                       years[1], "_", years[2], 
                       ".png")), 
           height = 2*n, 
           width = 8, 
           dpi = 600)
  }
}


#### NO23 contour plot ####
data_to_plot <- data_remix %>% 
  filter(Locator %in% stations, 
         !is.na(NO23)) %>% 
  group_by(Locator, Year, FakeYearDay, BinDepth) %>% 
  summarize(NO23 = mean(NO23, na.rm = TRUE)) %>% 
  ungroup()

if (all_stations_fig) {
  min_lim <- round_any(min(data_to_plot$NO23, na.rm = T),
                       accuracy = acc_NO23, f = floor)
  max_lim <- round_any(max(data_to_plot$NO23, na.rm = T),
                       accuracy = acc_NO23, f = ceiling)
  mybreaks <- seq(min_lim, max_lim, by = acc_NO23)
  mylabels <- mybreaks
  mylabels[!(round(mylabels, 2) == round(round(mylabels, 1), 2))] <- ""
  
  ggplot(data = data_to_plot) + 
    theme_classic() + 
    facet_wrap(~ Locator, 
               ncol = 1, 
               scales = "free_y") + 
    metR::geom_contour2(aes(x = FakeYearDay, y = BinDepth, z = NO23), 
                        na.fill = T, breaks = 0.034, color = "red") + 
    metR::geom_contour_fill(aes(x = FakeYearDay, 
                                y = BinDepth, 
                                z = NO23), 
                            na.fill = TRUE, 
                            breaks = mybreaks) + 
    scale_fill_cmocean(name = "solar", 
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
    geom_vline(aes(xintercept = FakeYearDay), 
               alpha = 0.2) + 
    labs(x = "", 
         y = "Depth (m)", 
         fill = "mg/L", 
         title = "NO2+3")
  ggsave(here("figs", "contour", "NO23", 
              paste0(paste(stations, collapse = "_"), 
                     "_NO23_", 
                     years[1], "_", years[2], 
                     ".png")), 
         height = 2*length(stations), 
         width = 8, 
         dpi = 600)
} else {
  for (station in stations) {
    temp <- data_to_plot %>% filter(Locator == station)
    
    min_lim <- round_any(min(temp$NO23, na.rm = T),
                         accuracy = acc_NO23, f = floor)
    max_lim <- round_any(max(temp$NO23, na.rm = T),
                         accuracy = acc_NO23, f = ceiling)
    mybreaks <- seq(min_lim, max_lim, by = acc_NO23)
    mylabels <- mybreaks
    mylabels[!(round(mylabels, 2) == round(round(mylabels, 1), 2))] <- ""
    
    ggplot(data = temp) + 
      theme_classic() + 
      facet_wrap(~ Year, 
                 ncol = 1) + 
      metR::geom_contour2(aes(x = FakeYearDay, y = BinDepth, z = NO23), 
                          na.fill = T, breaks = 0.034, color = "red") + 
      metR::geom_contour_fill(aes(x = FakeYearDay, 
                                  y = BinDepth, 
                                  z = NO23), 
                              na.fill = TRUE, 
                              breaks = mybreaks) + 
      # scale_fill_cmocean(name = "solar", 
      #                    breaks = mybreaks, 
      #                    limits = c(min_lim, max_lim), 
      #                    labels = mylabels, 
      #                    guide = guide_colorbar(show.limits = T, ticks = F), 
      #                    direction = -1) +
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
      geom_vline(aes(xintercept = FakeYearDay), 
                 alpha = 0.2) + 
      labs(x = "", 
           y = "Depth (m)", 
           fill = "mg/L", 
           title = paste(station, "NO23"))
    ggsave(here("figs", "contour", "NO23", station, 
                paste0(station, 
                       "_NO23_", 
                       years[1], "_", years[2], 
                       ".png")), 
           height = 2*n, 
           width = 8, 
           dpi = 600) 
  }
}

