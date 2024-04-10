#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)

yoi <- 2023

#### Load data ####
# Remember to update file via Socrata download first!
raw_data <- load_coupeville()

# Remember to update discrete data first!
# download_whidbey_discrete()
bottle_data <- load_whidbey_discrete()

#### Load/generate tide data ####
tide_data <- tide_height(stations = "Seattle", 
                         minutes = 15, 
                         from = min(raw_data$Date) - 1, 
                         to = max(raw_data$Date) + 1, 
                         tz = "Etc/GMT-8")
comb_data <- left_join(raw_data, tide_data %>% select(-Station))

#### Mooring - temperature salinity plot ####
ggplot(data = raw_data %>% 
         filter(Year == yoi), 
       aes(x = Temperature, 
           y = Salinity, 
           color = as.factor(Month))) + 
  theme_bw() + 
  geom_point(alpha = 0.5) +
  scale_color_viridis_d(begin = 0.2, 
                        direction = -1) + 
  labs(x = expression("Temperature " ( degree*C)), 
       y = "Salinity (PSU)", 
       color = "Month", 
       title = paste("Coupeville", yoi)) + 
  lims(x = c(4, 22), 
       y = c(0, 30))
ggsave(here("figs", "coupeville", paste0("T_S_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - temperature over time ####
ggplot(data = raw_data %>% 
         filter(Year <= yoi), 
       aes(x = FakeDateTime, 
           y = Temperature, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") +  
  geom_point() + 
  labs(x = "", 
       y = expression("Temperature " ( degree*C)), 
       title = paste("Coupeville", yoi), 
       color = "") + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  lims(y = c(0, NA))
ggsave(here("figs", "coupeville", paste0("T_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - salinity over time ####
ggplot(data = raw_data %>% 
         filter(Year <= yoi), 
       aes(x = FakeDateTime, 
           y = Salinity, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point() + 
  labs(x = "", 
       y = "Salinity (PSU)", 
       title = paste("Coupeville", yoi), 
       color = "") + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  lims(y = c(0, 31))
ggsave(here("figs", "coupeville", paste0("S_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - nitrate over time ####
ggplot(data = raw_data %>% 
         filter(Year <= yoi), 
       aes(x = FakeDateTime, 
           y = NO23, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point() + 
  labs(y = "SUNA Parameter 2", 
       x = "", 
       color = "", 
       title = paste("Coupeville", yoi)) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  lims(y = c(0, NA)) + 
  scale_x_datetime(date_breaks = "1 month", 
               date_labels = "%b")
ggsave(here("figs", "coupeville", paste0("NO23_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - DO over time ####
ggplot(data = raw_data %>% 
         filter(Year <= yoi), 
       aes(x = FakeDateTime, 
           y = Oxygen, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point(alpha = 0.5, size = 1) + 
  labs(y = "DO (mg/L)", 
       x = "", 
       color = "", 
       title = paste("Coupeville", yoi)) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  lims(y = c(0, NA))
ggsave(here("figs", "coupeville", paste0("DO_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - chl over time ####
ggplot(data = raw_data %>% 
         filter(Year <= yoi), 
       aes(x = FakeDateTime, 
           y = Chlorophyll, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point() + 
  labs(x = "", 
       y = expression(Chlorophyll~(mu*g/L)), 
       color = "", 
       title = paste("Coupeville", yoi)) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  lims(y = c(0, 100))
ggsave(here("figs", "coupeville", paste0("chl_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - turbidity over time ####
ggplot(data = raw_data %>% 
         filter(Turbidity < 100), 
       aes(x = FakeDateTime, 
           y = Turbidity, 
           color = Year == yoi)) + 
  theme_bw() +
  theme(legend.position = "none") + 
  geom_point() + 
  labs(x = "", 
       y = "Turbidity (NTU)", 
       color = "") + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray"))
ggsave(here("figs", "coupeville", paste0("turb_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - pressure and tide height over a month ####
ggplot(data = comb_data %>% 
         filter(Year == yoi, 
                Month == 4), 
       aes(x = DateTime)) + 
  theme_bw() + 
  geom_point(aes(y = Pressure)) + 
  geom_point(aes(y = TideHeight), color = "red") + 
  labs(x = "", 
       y = "Pressure/depth")

#### Mooring - pressure vs tide height over a month ####
ggplot(data = comb_data %>% 
         filter(Year == yoi, 
                Month == 4), 
       aes(x = Pressure, 
           y = TideHeight)) + 
  theme_bw() + 
  geom_point() + 
  labs(x = "Pressure (db)", 
       y = "Tide Height (m)")
ggsave(here("figs", "coupeville", paste0("pressure_vs_tide_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - pressure vs. salinity ####
ggplot(data = raw_data %>% 
         filter(!Pressure < 0, 
                Year == yoi), 
       aes(x = Pressure, 
           y = Salinity)) + 
  theme_bw() + 
  facet_wrap(~ Month) + 
  geom_point(alpha = 0.5) + 
  scale_color_viridis_d(begin = 0.2, 
                        direction = -1)

#### Bottle - nitrate over time ####
ggplot(data = bottle_data %>% 
         filter(Locator == "PENNCOVECW", 
                ParmId == 14), 
       aes(x = FakeDate, 
           y = Value, 
           shape = Detect, 
           color = Year == yoi)) + 
  theme_bw() + 
  facet_wrap(~ DepthBin, 
             ncol = 1) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray"), 
                     labels = c("TRUE" = yoi, 
                                "FALSE" = paste(min(bottle_data$Year), 
                                                yoi-1, 
                                                sep = "-"))) + 
  scale_shape_manual(values = c("TRUE" = 16, 
                                "FALSE" = 1)) + 
  labs(x = "", 
       y = expression("NO"[2]^"-"~"+"~"NO"[3]^"-"~"(mg N/L)"), 
       shape = "Detect", 
       color = "Year")
ggsave(here("figs", "coupeville", paste0("bottle_N03_", yoi, ".png")), 
       height = 5, width = 8)

#### Bottle - ammonia over time ####
ggplot(data = bottle_data %>% 
         filter(Locator == "PENNCOVECW", 
                ParmId == 13), 
       aes(x = FakeDate, 
           y = Value, 
           shape = Detect, 
           color = Year == yoi)) + 
  theme_bw() + 
  facet_wrap(~ DepthBin, 
             ncol = 1) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray"), 
                     labels = c("TRUE" = yoi, 
                                "FALSE" = paste(min(bottle_data$Year), 
                                                yoi-1, 
                                                sep = "-"))) + 
  scale_shape_manual(values = c("TRUE" = 16, 
                                "FALSE" = 1)) + 
  labs(x = "", 
       y = expression("NH"[3]~"(mg N/L)"), 
       shape = "Detect", 
       color = "Year")
ggsave(here("figs", "coupeville", paste0("bottle_NH3_", yoi, ".png")), 
       height = 5, width = 8)

#### Bottle - phosphate over time ####
ggplot(data = bottle_data %>% 
         filter(Locator == "PENNCOVECW", 
                ParmId == 15), 
       aes(x = FakeDate, 
           y = Value, 
           shape = Detect, 
           color = Year == yoi)) + 
  theme_bw() + 
  facet_wrap(~ DepthBin, 
             ncol = 1) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray"), 
                     labels = c("TRUE" = yoi, 
                                "FALSE" = paste(min(bottle_data$Year), 
                                                yoi-1, 
                                                sep = "-"))) + 
  scale_shape_manual(values = c("TRUE" = 16, 
                                "FALSE" = 1)) + 
  labs(x = "", 
       y = expression("PO"[4]^"3-"~"(mg P/L)"), 
       shape = "Detect", 
       color = "Year")
ggsave(here("figs", "coupeville", paste0("bottle_PO4_", yoi, ".png")), 
       height = 5, width = 8)

