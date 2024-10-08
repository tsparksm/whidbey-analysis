#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(waterData)
library(rtide)

#### Load and combine old (pre-Dec 2023) buoy data #### 
data_buoy <- load_PS_buoy_old() %>% 
  mutate(Type = "KC") %>% 
  filter(Pressure > 0.5) %>% 
  select(-Turbidity, 
         -TurbiditySD, 
         -ChlorophyllSD, 
         -EventFlags, 
         -Pressure) 
data_buoy_ST <- load_PS_buoy_ST() %>% 
  mutate(Type = "ST")
data_buoy_new <- load_psusan() %>% 
  mutate(Type = "KC")
data_buoy_comb <- full_join(data_buoy, data_buoy_ST)
data_buoy_comb <- full_join(data_buoy_comb, data_buoy_new) %>% 
  mutate(Year = year(DateTime), 
         Month = month(DateTime))
data_buoy_avg <- data_buoy_comb %>% 
  group_by(DateTime) %>% 
  summarize(Temperature = mean(Temperature, na.rm = TRUE), 
            Salinity = mean(Salinity, na.rm = TRUE), 
            Oxygen = mean(Oxygen, na.rm = TRUE), 
            Chlorophyll = mean(Chlorophyll, na.rm = TRUE), 
            NO23 = mean(NO23, na.rm = TRUE))

#### Load discrete and CTD data ####
data_discrete <- load_whidbey_discrete() %>% 
  mutate(Type = "bottle", 
         DateTime = CollectDateTime) %>% 
  filter(Locator == "PSUSANBUOY")

data_ctd <- load_CTD("PSUSANBUOY")

#### Load climate, tide, and river data ####
station <- "USW00024222"  # Everett airport
data_climate <- load_climate() %>% 
  filter(STATION == station) %>% 
  rename(Date = DATE) %>% 
  mutate(Year = year(Date))

data_river <- importDVs("12167000", 
                        sdate = "2022-01-01") %>% 
  mutate(River = "Stillaguamish") %>% 
  rename(Date = dates, 
         Flow = val) %>% 
  select(Date, Flow)

data_tide <- tide_height(stations = "Seattle", 
                         minutes = 15, 
                         from = as.Date("2022-01-01"), 
                         to = as.Date("2022-12-31"), 
                         tz = "Etc/GMT+8")

#### QC ####
# To do: put this somewhere else, save output
data_buoy_long <- data_buoy_comb %>% 
  pivot_longer(cols = Temperature:OxygenSat, 
               names_to = "Parameter", 
               values_to = "Value") %>% 
  add_column(Flag = 0)

# ggplotly time!
# yoi <- 2013
# parm <- "Temperature"
# g <- ggplot(data = data_buoy_long %>% 
#          filter(Parameter == parm, 
#                 year(DateTime) == yoi), 
#        aes(x = DateTime, 
#            y = Value, 
#            color = Type)) + 
#   geom_point() + 
#   labs(title = paste(parm, yoi))
# ggplotly(g)

# Add flags
data_buoy_qc <- data_buoy_long %>% 
  # Flag all 2022 KC chl data
  mutate(Flag = ifelse(Type == "KC" & 
                         Parameter == "Chlorophyll" & 
                         year(DateTime) %in% 2022:2023, 
                       4, Flag)) %>% 
  # Flag aberrant 2022 salinity points
  mutate(Flag = ifelse(Type == "KC" & 
                         Parameter == "Salinity" & 
                         year(DateTime) == 2022 & 
                         yday(DateTime) == 271 & 
                         Value == 0.5797, 
                       4, Flag), 
         Flag = ifelse(Type == "ST" & 
                         Parameter == "Salinity" & 
                         year(DateTime) == 2022 & 
                         yday(DateTime) == 257 & 
                         Value == 0.4400, 
                       4, Flag)) %>% 
  # Flag aberrant 2023 ST points %>% 
  mutate(Flag = ifelse(Type == "ST" & 
                         round_date(DateTime, unit = "15 minutes") %in% as.POSIXct(
                           c("2023-03-15 11:45", 
                             "2023-03-15 10:00", 
                             "2023-08-11 14:00", 
                             "2023-06-27 11:00"), 
                           tz = "Etc/GMT+8"), 
                           4, Flag)) %>% 
  # Flag August-Sep 2023 KC DO %>%
  mutate(Flag = ifelse(Type == "KC" & 
                         Parameter == "Oxygen" & 
                         between(DateTime, 
                                 as.POSIXct("2023-08-12 13:00", tz = "Etc/GMT+8"), 
                                 as.POSIXct("2023-09-13 15:15", tz = "Etc/GMT+8")), 
                       4, Flag)) %>% 
  # Flag aberrant 2018 salinity points
  mutate(Flag = ifelse(Parameter == "Salinity" & 
                         DateTime %in% seq(as.POSIXct("2018-10-15 12:00:00", 
                                                      tz = "Etc/GMT+8"), 
                                           as.POSIXct("2018-10-15 13:00:00", 
                                                      tz = "Etc/GMT+8"), 
                                           by = 60*30), 
                       4, Flag)) %>% 
  # Flag 2019 chlorophyll points
  mutate(Flag = ifelse(Parameter == "Chlorophyll" & 
                         DateTime %in% seq(as.POSIXct("2019-05-28 06:00:00", 
                                                      tz = "Etc/GMT+8"), 
                                           as.POSIXct("2019-06-06 07:30:00", 
                                                      tz = "Etc/GMT+8"), 
                                           by = 60*30), 
                       4, Flag)) %>% 
  # Flag salinity above 32 or below 0
  mutate(Flag = ifelse(Parameter == "Salinity" & 
                         (Value >= 32 | Value < 0), 
                       4, Flag)) %>% 
  # Flag chlorophyll above 100 or below 0
  mutate(Flag = ifelse(Parameter == "Chlorophyll" &
                         (Value >= 100 | Value < 0), 
                       4, Flag))

write_csv(data_buoy_qc, 
          here("data", "port_susan_buoy_qc.csv"))

#### Figure - chlorophyll, all years ####
yoi <- 2023

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Chlorophyll", 
         year(DateTime) <= yoi, 
         !(Flag %in% 3:4)) %>% 
  mutate(YearGroup = ifelse(year(DateTime) == yoi, 
                            yoi, 
                            paste(min(year(DateTime)), 
                                  yoi-1, 
                                  sep = "-")), 
         FakeDate = DateTime) %>% 
  arrange(YearGroup)
year(data_to_plot$FakeDate) <- yoi

ggplot(data_to_plot, 
       aes(x = FakeDate, 
           y = Value, 
           color = YearGroup, 
           shape = Type)) + 
  geom_point()

#### Figure - chlorophyll, single year ####
yoi <- 2023

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Chlorophyll", 
         year(DateTime) == yoi)

data_to_plot2 <- data_discrete %>% 
  filter(ParmId == 1, 
         year(CollectDate) == yoi)

ggplot() + 
  geom_point(data = data_to_plot, 
             aes(x = DateTime, 
                 y = Value, 
                 color = Type)) + 
  geom_point(data = data_to_plot2, 
             aes(x = CollectDateTime, 
                 y = Value), 
             color = "red") + 
  theme_bw() + 
  labs(x = "", 
       y = expression(Chlorophyll~(mu*g/L)), 
       title = paste("Pt. Susan buoy", 2023)) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b")
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_chlorophyll.png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - oxygen, all years ####
yoi <- 2023

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Oxygen", 
         year(DateTime) <= yoi, 
         !(Flag %in% 3:4)) %>% 
  mutate(YearGroup = ifelse(year(DateTime) == yoi, 
                            yoi, 
                            paste(min(year(DateTime)), 
                                  yoi-1, 
                                  sep = "-")), 
         FakeDate = DateTime) %>% 
  arrange(YearGroup)
year(data_to_plot$FakeDate) <- yoi

ggplot(data_to_plot, 
       aes(x = FakeDate, 
           y = Value, 
           color = YearGroup, 
           shape = Type)) + 
  geom_point()


#### Figure - oxygen, single year ####
yoi <- 2023

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Oxygen", 
         year(DateTime) == yoi, 
         Type == "KC")

data_to_plot2 <- data_discrete %>% 
  filter(ParmId %in% 5:6, 
         year(CollectDate) == yoi)

ggplot() + 
  geom_point(data = data_to_plot, 
             aes(x = DateTime, 
                 y = Value)) + 
  geom_point(data = data_to_plot2, 
             aes(x = CollectDateTime, 
                 y = Value), 
             color = "red") + 
  theme_bw() + 
  labs(x = "", 
       y = "Oxygen (mg/L)")
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_DO.png")), 
       dpi = 600, height = 6, width = 4)

#### Figure - nitrate, single year ####
yoi <- 2023

data_to_plot <- data_buoy_new %>% 
  filter(year(DateTime) == yoi)

ggplot(data_to_plot, 
       aes(x = DateTime, 
           y = NO23)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "", 
       y = expression("NO"[2]^"-"~"+"~"NO"[3]^"-"~"(mg N/L)"))
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_no23.png")), 
       dpi = 600, height = 6, width = 4)

#### Figure - daily mean temperature ####
yoi <- 2023

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Temperature", 
         year(DateTime) <= yoi, 
         !(Flag %in% 3:4)) %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  group_by(Date) %>% 
  summarize(Mean = mean(Value)) %>% 
  ungroup() %>% 
  mutate(FakeDate = Date)
year(data_to_plot$FakeDate) <- yoi

data_avg <- data_to_plot %>% 
  group_by(FakeDate) %>% 
  summarize(AllMean = mean(Mean, na.rm = TRUE), 
            Meanplus = AllMean + 2*sd(Mean, na.rm = TRUE), 
            Meanminus = AllMean - 2*sd(Mean, na.rm = TRUE))

ggplot() + 
  theme_bw() + 
  geom_ribbon(
    data = data_avg, 
    aes(x = FakeDate, 
        ymin = Meanminus, 
        ymax = Meanplus, 
        color = "L1"), 
    fill = "gray") + 
  geom_line(
    data = data_to_plot %>% filter(year(Date) == yoi), 
    aes(x = FakeDate, y = Mean, color = "L2"), 
    linewidth = 1.2, 
    fill = NA
  ) + 
  geom_line(
    data = data_avg, 
    aes(x = FakeDate, 
        y = AllMean, 
        color = "L3"), 
    fill = NA,
  ) + 
  scale_color_manual(
    values = c("L1" = "gray", 
      "L2" = "black", 
      "L3" = "red"), 
    labels = c(
      paste0("2011-", yoi-1), 
      yoi, 
      "Mean"
    )
  ) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") + 
  labs(x = "", 
       y = expression("Temperature " ( degree*C)), 
       title = "Pt. Susan buoy", 
       color = "")
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_daily_mean_T.png")), 
       dpi = 600, height = 4, width = 6)

#### River flow ####
data_to_plot <- data_buoy_qc %>% 
  mutate(Date = as.Date.POSIXct(DateTime, 
                                tz = "Etc/GMT+8")) %>% 
  filter(!(Flag %in% 3:4), 
         !is.na(Value)) %>% 
  group_by(Date, Parameter) %>% 
  summarize(Mean = mean(Value), 
            Median = median(Value), 
            Max = max(Value), 
            Min = min(Value), 
            SD = sd(Value), 
            n = n()) %>% 
  left_join(data_river)

ggplot(data = data_to_plot %>% 
         filter(Parameter == "Salinity"), 
       aes(x = Flow, 
           y = Mean)) + 
  theme_bw() + 
  geom_point()




