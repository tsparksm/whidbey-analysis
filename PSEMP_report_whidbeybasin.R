#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(metR)
library(cmocean)
library(pracma)
library(RColorBrewer)
library(patchwork)

yoi <- 2022

# Figure settings
fig_dpi <- 600
fig_height <- 12
fig_width <- 8
font_size <- 12

# Plot settings
palette_mooring <- c("#bebebe", "black")
shapes_mooring <- c(8, 16)
shapes_mdl <- c(16, 1)
glob_lab_scale <- 5/14
point_size <- 5
sigmat_contour_alpha <- 0.1  # if you want contour lines, use 0.1; else use 0

# Contour plot settings
# acc_T <- 0.2
acc_sigmaT <- 0.2
# acc_S <- 0.1
# acc_DO <- 0.2
# acc_chl <- 0.5
# acc_N <- 0.05

#### Load data ####
data_discrete <- load_whidbey_discrete() %>% 
  mutate(Type = "bottle", 
         Year = year(CollectDate)) %>% 
  rename(Date = CollectDate)

bin_width <- 0.5
data_ctd <- load_composite(bin_width, 
                           monthly = FALSE) %>% 
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

#### Sigma-t contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(SigmaTheta), 
         Locator == "SARATOGACH") %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(SigmaTheta = mean(SigmaTheta)) %>% 
  ungroup()

lims <- c(20, 23.6)
mybreaks <- seq(lims[1], lims[2], by = acc_sigmaT)
mylabels <- mybreaks
mylabels[!(round(mylabels, 2) == round(round(mylabels, 2)))] <- ""
mylabels[1] <- paste0("<", lims[1])
mylabels[length(mylabels)] <- paste0(">", lims[2])

data_to_plot <- data_to_plot %>% 
  mutate(SigmaTheta = ifelse(SigmaTheta >= lims[2], 
                             23.6 - 1e-3, 
                             SigmaTheta))

p1 <- ggplot(data = data_to_plot) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank()) + 
  metR::geom_contour_fill(aes(x = YearDay, 
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
  geom_vline(aes(xintercept = YearDay), 
             alpha = 0.2) + 
  labs(x = "", 
       y = "Depth (m)", 
       fill = expression(kg/m^3), 
       title = expression(A.~Camano~Head~sigma [theta]~density))

#### N bottle ####
data_to_plot <- data_discrete %>% 
  filter(ParmId == 14, 
         DepthBin == "surface", 
         Year == yoi)

p2 <- ggplot(data = data_to_plot, 
             aes(x = as.Date(Date), 
                 y = Value)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank(), 
        legend.position = "right") + 
  geom_point(aes(shape = grepl("MDL", QfrCode), 
                 color = Locator)) + 
  geom_smooth(color = "black", se = FALSE) + 
  scale_x_date(limits = as.Date(c(paste0(yoi, "-01-01"), 
                                  paste0(yoi, "-12-31"))), 
               expand = c(0, 0), 
               date_breaks = "1 month") + 
  scale_shape_manual(values = shapes_mdl, 
                     guide = "none") + 
  scale_color_manual(values = brewer.pal(6, "YlGnBu")[2:6]) + 
  labs(x = "", 
       y = "Nitrate + nitrite N (mg/L)", 
       title = "B. Surface nitrate concentration - bottle stations", 
       color = "")

#### Bottom DO ####
stations <- c("SARATOGARP", "SARATOGAOP", "SARATOGACH", 
              "PSUSANKP", "PSUSANENT", "Poss DO-2")

data_to_plot <- data_ctd %>% 
  filter(Year == yoi, 
         Locator %in% stations) %>% 
  group_by(Locator, Date) %>% 
  summarize(MinDO = min(DO))

p3 <- ggplot(data = data_to_plot, 
             aes(x = as.Date(Date), 
                 y = MinDO)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_blank(), 
        legend.position = "right") + 
  geom_point(aes(color = Locator)) + 
  geom_smooth(color = "black", se = FALSE) + 
  scale_x_date(limits = as.Date(c(paste0(yoi, "-01-01"), 
                                  paste0(yoi, "-12-31"))), 
               expand = c(0, 0), 
               date_breaks = "1 month") + 
  scale_shape_manual(values = shapes_mdl) + 
  scale_color_brewer(palette = "Paired") + 
  labs(x = "", 
       y = "DO (mg/L)", 
       color = "", 
       title = "C. Minimum dissolved oxygen - deep stations")

#### Integrated chl ####
stations <- c("SARATOGARP", "SARATOGAOP", "SARATOGACH", 
              "PSUSANKP", "PSUSANENT", "Poss DO-2")

totalchl <- data_ctd %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Year == yoi, 
         Locator %in% stations) %>% 
  group_by(Locator, Date) %>% 
  summarize(Int_chl = trapz(Depth, Chlorophyll))

p4 <- ggplot(data = totalchl, 
             aes(x = Date, 
                 y = Int_chl, 
                 color = Locator)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.text.x = element_text(size = font_size + 2, face = "bold"), 
        legend.position = "none") + 
  geom_point() + 
  geom_line() + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_date(limits = as.Date(c(paste0(yoi, "-01-01"), 
                                  paste0(yoi, "-12-31"))), 
               expand = c(0, 0), 
               date_breaks = "1 month", 
               date_labels = "%b") + 
  labs(x = "", 
       y = expression(Chl~a~(mg/m^2)), 
       color = "", 
       title = "D. 1-50 m integrated chlorophyll a - deep stations")

#### Put it all together ####
p0 <- (p3 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) / 
  (p4 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) + 
  plot_layout(guides = "collect")
pp0 <- p1 / 
  (p2 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) / 
  p0 + plot_layout(heights = c(1, 2, 4))
# pp <- p1 / 
#   (p2 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) / 
#   (p3 + theme(plot.margin = unit(c(0,30,0,0), "pt"))) / 
#   p4 + 
#   plot_layout(heights = c(1, 2, 2, 2))
ggsave(here("figs", paste0("PSEMP_whidbey_", yoi, ".png")), 
       pp0, 
       dpi = fig_dpi, 
       height = fig_height, 
       width = fig_width)
