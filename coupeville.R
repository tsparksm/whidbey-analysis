#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)

yoi <- 2024

#### Load data ####
# Remember to update file via Socrata download first!
qc_data <- load_qc_coupeville()
raw_data <- load_coupeville()

# Remember to update discrete data first!
# download_whidbey_discrete()
bottle_data <- load_whidbey_discrete()

ctd_data <- load_CTD("PENNCOVECW")

#### Load/generate tide data ####
tide_data <- tide_height(stations = "Seattle", 
                         minutes = 15, 
                         from = min(qc_data$Date) - 1, 
                         to = max(qc_data$Date) + 1, 
                         tz = "Etc/GMT-8")
comb_data <- left_join(qc_data, tide_data %>% select(-Station))

#### Mooring - temperature salinity plot ####
data_to_plot <- qc_data %>%  
  filter(Year == yoi) %>% 
  mutate(Temperature = ifelse(Temperature_final == 1, 
                              Temperature, NA), 
         Salinity = ifelse(Salinity_final == 1, 
                           Salinity, NA))
ggplot(data = data_to_plot, 
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
       title = paste("Coupeville", yoi))
ggsave(here("figs", "coupeville", paste0("T_S_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - temperature over time ####
data_to_plot <- qc_data %>%  
  filter(Year <= yoi) %>% 
  mutate(Temperature = ifelse(Temperature_final == 1, 
                              Temperature, NA))
ggplot(data = data_to_plot, 
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
data_to_plot <- qc_data %>%  
  filter(Year <= yoi) %>% 
  mutate(Salinity = ifelse(Salinity_final == 1, 
                           Salinity, NA))
ggplot(data = data_to_plot, 
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
                   date_labels = "%b")
ggsave(here("figs", "coupeville", paste0("S_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - salinity short period ####
beg_date <- as.Date("2024-11-01")
end_date <- as.Date("2024-11-30")
ggplot(data = qc_data %>% 
         filter(between(Date, beg_date, end_date)) %>% 
         mutate(Salinity = ifelse(Salinity_final == 1, 
                                  Salinity, NA)), 
       aes(x = DateTime, 
           y = Salinity)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point(alpha = 0.5, size = 1) + 
  labs(y = "Salinity (PSU)", 
       x = "", 
       color = "", 
       title = "Coupeville") + 
  scale_x_datetime(date_breaks = "1 week", 
                   date_labels = "%m-%d-%y")
ggsave(here("figs", "coupeville", 
            paste0("S_", beg_date, "_", end_date, ".png")), 
       height = 4, width = 6)

#### Mooring - nitrate over time ####
ggplot(data = qc_data %>% 
         filter(Year <= yoi) %>% 
         mutate(NO23 = ifelse(NO23_final == 1, NO23, NA)), 
       aes(x = FakeDateTime, 
           y = NO23, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point() + 
  labs(y = expression("NO"[2]^"-"~"+"~"NO"[3]^"-"~"(mg N/L)"), 
       x = "", 
       color = "", 
       title = paste("Coupeville", yoi)) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  lims(y = c(0, 0.5)) + 
  scale_x_datetime(date_breaks = "1 month", 
               date_labels = "%b")
ggsave(here("figs", "coupeville", paste0("NO23_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - DO over time ####
ggplot(data = qc_data %>% 
         filter(Year <= yoi) %>% 
         mutate(Oxygen = ifelse(Oxygen_final == 1, Oxygen, NA)), 
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

#### Mooring - DO short period ####
beg_date <- as.Date("2024-11-01")
end_date <- as.Date("2024-11-30")
ggplot(data = qc_data %>% 
         filter(between(Date, beg_date, end_date)) %>% 
         mutate(Oxygen = ifelse(Oxygen_final == 1, Oxygen, NA)), 
       aes(x = DateTime, 
           y = Oxygen)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point(alpha = 0.5, size = 1) + 
  labs(y = "DO (mg/L)", 
       x = "", 
       color = "", 
       title = "Coupeville") + 
  scale_x_datetime(date_breaks = "1 week", 
                   date_labels = "%m-%d-%y")
ggsave(here("figs", "coupeville", 
            paste0("DO_", beg_date, "_", end_date, ".png")), 
       height = 4, width = 6)

#### Mooring - chl over time ####
ggplot(data = qc_data %>% 
         filter(Year <= yoi) %>% 
         mutate(Chlorophyll = ifelse(Chlorophyll_final == 1, 
                                     Chlorophyll, NA)), 
       aes(x = FakeDateTime, 
           y = Chlorophyll, 
           color = Year == yoi)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point() + 
  labs(x = "", 
       y = expression(Chlorophyll~fluorescence~(mu*g/L)), 
       color = "", 
       title = paste("Coupeville", yoi)) + 
  scale_color_manual(values = c("TRUE" = "black", 
                                "FALSE" = "gray")) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b")
ggsave(here("figs", "coupeville", paste0("chl_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - turbidity over time ####
ggplot(data = qc_data %>% 
         mutate(Turbidity = ifelse(Turbidity_final == 1, Turbidity, NA)) %>% 
         filter(Year <= yoi), 
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
                Month == 9) %>% 
         mutate(Pressure = ifelse(Pressure_final == 1, Pressure, NA)), 
       aes(x = DateTime)) + 
  theme_bw() + 
  geom_point(aes(y = Pressure)) + 
  geom_point(aes(y = TideHeight), color = "red") + 
  labs(x = "", 
       y = "Pressure/depth")

#### Mooring - pressure vs tide height over a month ####
ggplot(data = comb_data %>% 
         filter(Year == yoi, 
                Month == 9) %>% 
         mutate(Pressure = ifelse(Pressure_final == 1, Pressure, NA)), 
       aes(x = Pressure, 
           y = TideHeight)) + 
  theme_bw() + 
  geom_point() + 
  labs(x = "Pressure (db)", 
       y = "Tide Height (m)")
ggsave(here("figs", "coupeville", paste0("pressure_vs_tide_", yoi, ".png")), 
       height = 4, width = 6)

#### Mooring - pressure vs. salinity ####
ggplot(data = qc_data %>% 
         filter(Year == yoi) %>% 
         mutate(Pressure = ifelse(Pressure_final == 1, Pressure, NA), 
                Salinity = ifelse(Salinity_final == 1, Salinity, NA)), 
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
                ParmId == 14, 
                Year <= yoi, 
                DepthBin != "7-10 m") %>% 
         arrange(Year), 
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
                ParmId == 13, 
                Year <= yoi) %>% 
         arrange(Year), 
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
                ParmId == 15, 
                Year <= yoi) %>% 
         arrange(Year),
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

