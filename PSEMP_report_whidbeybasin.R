#### SETUP ####
source(here::here("src", "utility_functions.R"))
source(here::here("src", "contour_functions.R"))
library(metR)
library(cmocean)
library(pracma)
library(RColorBrewer)
library(patchwork)
library(ggnewscale)

yoi <- 2025

# Figure settings
fig_dpi <- 600
fig_height <- 12
fig_width <- 8
fig_point_size <- 4
font_size <- 12

# Plot settings
palette_mooring <- c("#bebebe", "black")
shapes_mooring <- c(8, 16)
shapes_mdl <- c(16, 1)
glob_lab_scale <- 5/14
point_size <- 5
sigmat_contour_alpha <- 0.1  # if you want contour lines, use 0.1; else use 0

# Contour plot settings
acc_T <- 0.2
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
data_ctd_init <- load_composite(bin_width, 
                           monthly = FALSE) %>% 
  mutate(YearDay = yday(Date), 
         Year = year(Date))

# Add extra CTD data before/after each year
extra_data_before <- data_ctd_init %>% 
  filter(Year == yoi - 1, 
         Locator == "SARATOGACH") %>% 
  filter(YearDay == max(YearDay)) %>% 
  mutate(YearDay = YearDay - 365, 
         Year = yoi)
data_ctd <- add_row(data_ctd_init, extra_data_before)

extra_data_after <- data_ctd_init %>% 
  filter(Year == yoi + 1, 
         Locator == "SARATOGACH") %>% 
  filter(YearDay == min(YearDay)) %>% 
  mutate(YearDay = YearDay + 365, 
         Year = yoi)
data_ctd <- add_row(data_ctd, extra_data_after) %>% 
  filter(Year == yoi)

#### T contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(Temperature), 
         Locator == "SARATOGACH") %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(Temperature = mean(Temperature)) %>% 
  ungroup()

lims <- get_limits(data_to_plot$Temperature, acc_T)
mybreaks <- seq(lims[1], lims[2], by = acc_T)
mylabels <- get_labels(mybreaks, even_only = TRUE)

data_to_plot <- data_to_plot |> 
  rename(FakeYearDay = YearDay)

p1 <- ggplot(data = data_to_plot) + 
  add_t_contour() + 
  theme_bw() + 
  theme(
    text = element_text(size = font_size), 
    axis.title.y = element_text(size = font_size + 1), 
    axis.text.y = element_text(size = font_size + 1), 
    axis.text.x = element_blank()
  ) + 
  labs(
    fill = expression(degree*C), 
    title = "A. Camano Head water temperature"
  )

#### Sigma-t contour ####
data_to_plot <- data_ctd %>% 
  filter(!is.na(SigmaTheta), 
         Locator == "SARATOGACH") %>% 
  group_by(Locator, Year, YearDay, BinDepth) %>% 
  summarize(SigmaTheta = mean(SigmaTheta)) %>% 
  ungroup()

lims <- c(20, 23.6)
mybreaks <- seq(lims[1], lims[2], by = acc_sigmaT)
mylabels <- get_labels(mybreaks, min_lim = lims[1], max_lim = lims[2])

data_to_plot <- data_to_plot %>% 
  mutate(
    SigmaTheta = case_when(
      SigmaTheta >= lims[2] ~ lims[2] - 1e-3, 
      SigmaTheta <= lims[1] ~ lims[1] + 1e-3, 
      TRUE ~ SigmaTheta
      )
    ) |> 
  rename(FakeYearDay = YearDay)

# p1 <- ggplot(data = data_to_plot) +
#   add_sigmat_contour() +
#   theme_bw() +
#   theme(
#     text = element_text(size = font_size),
#     axis.title.y = element_text(size = font_size + 1),
#     axis.text.y = element_text(size = font_size + 1), 
#     axis.text.x = element_blank()
#   ) +
#   labs(
#     fill = expression(kg/m^3),
#     title = expression(A.~Camano~Head~sigma [theta]~density)
#   )

#### N bottle ####
data_to_plot <- data_discrete |> 
  filter(ParmId == 14, DepthBin == "surface", Year <= yoi) |> 
  mutate(
    FakeDate = Date, 
    PlotGroup = case_when(
      Year == yoi & grepl("MDL", QfrCode) ~ "nondetect", 
      Year == yoi ~ "yoi", 
      TRUE ~ "other"
    )
  )
year(data_to_plot$FakeDate) <- yoi

p2 <- ggplot(data = data_to_plot, aes(x = FakeDate, y = Value)) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    text = element_text(size = font_size), 
    axis.title.y = element_text(size = font_size + 1), 
    axis.text.y = element_text(size = font_size + 1), 
    axis.text.x = element_blank(), 
    legend.position = "right"
  ) + 
  geom_smooth(
    aes(color = Year == yoi, group = Year), 
    se = FALSE, 
    show.legend = FALSE
  ) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gray")) + 
  new_scale_color() + 
  geom_point(
    aes(shape = PlotGroup, color = Locator), 
    size = fig_point_size
  ) + 
  scale_x_date(
    limits = as.Date(c(paste0(yoi, "-01-01"), paste0(yoi, "-12-31"))), 
    expand = c(0, 0), 
    date_breaks = "1 month"
  ) + 
  scale_shape_manual(
    values = c("nondetect" = 1, "yoi" = 16, "other" = NA), 
    guide = "none"
  ) + 
  scale_color_manual(values = brewer.pal(6, "YlGnBu")[2:6]) + 
  labs(x = "", 
       y = "Nitrate + nitrite N (mg/L)", 
       title = "B. Surface nitrate + nitrite concentration", 
       color = "")

#### Bottom DO ####
stations <- c("SARATOGARP", "SARATOGAOP", "SARATOGACH", 
              "PSUSANKP", "PSUSANENT", "Poss DO-2")

data_to_plot <- data_ctd_init |>  
  filter(between(Year, 2022, yoi), Locator %in% stations) |> 
  group_by(Locator, Year, Date) |> 
  summarize(MinDO = min(DO)) |> 
  ungroup() |> 
  mutate(FakeDate = Date)
year(data_to_plot$FakeDate) <- yoi

p3 <- ggplot(data = data_to_plot, aes(x = FakeDate, y = MinDO)) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    text = element_text(size = font_size), 
    axis.title.y = element_text(size = font_size + 1), 
    axis.text.y = element_text(size = font_size + 1), 
    axis.text.x = element_blank(), 
    legend.position = "right"
  ) + 
  geom_smooth(
    aes(color = Year == yoi, group = Year), 
    se = FALSE, 
    show.legend = FALSE
  ) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gray")) + 
  new_scale_color() + 
  geom_point(
    aes(color = Locator, shape = Year == yoi), 
    size = fig_point_size
  ) + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = NA), guide = "none") + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_date(
    limits = as.Date(c(paste0(yoi, "-01-01"), paste0(yoi, "-12-31"))), 
    expand = c(0, 0), 
    date_breaks = "1 month"
  ) + 
  labs(x = "", 
       y = "DO (mg/L)", 
       color = "", 
       title = "C. Minimum dissolved oxygen - deep stations")


#### Integrated chl multiple years ####
stations <- c("SARATOGARP", "SARATOGAOP", "SARATOGACH", 
              "PSUSANKP", "PSUSANENT", "Poss DO-2")

totalchl <- data_ctd_init %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Locator %in% stations, 
         Year >= 2022) %>% 
  group_by(Locator, Year, Date) %>% 
  summarize(Int_chl = trapz(Depth, Chlorophyll)) |> 
  mutate(FakeDate = Date)
year(totalchl$FakeDate) <- yoi

p4 <- ggplot(data = totalchl, 
             aes(x = FakeDate, 
                 y = Int_chl)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = font_size), 
        axis.title.y = element_text(size = font_size + 1), 
        axis.text.y = element_text(size = font_size + 1), 
        axis.text.x = element_text(size = font_size + 2, face = "bold"), 
        legend.position = "none") + 
  geom_smooth(
    aes(color = Year == yoi, 
        group = Year), 
    se = FALSE
  ) + 
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gray")) + 
  new_scale_color() + 
  geom_point(
    aes(color = Locator, shape = Year == yoi), 
    size = fig_point_size
    ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = NA)) + 
  scale_x_date(limits = as.Date(c(paste0(yoi, "-01-01"), 
                                  paste0(yoi, "-12-31"))), 
               expand = c(0, 0), 
               date_breaks = "1 month", 
               date_labels = "%b") + 
  scale_y_continuous(limits = c(0, 175)) + 
  labs(x = "", 
       y = expression(Chl~a~fluorescence~(mg/m^2)), 
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
