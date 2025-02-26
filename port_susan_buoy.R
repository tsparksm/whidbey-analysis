#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(waterData)
library(rtide)
library(zoo)

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
yoi <- 2024

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
  theme_bw() + 
  geom_point() + 
  scale_color_manual(values = c("gray", "black")) + 
  labs(x = "", 
       y = expression(Chlorophyll~fluorescence~(mu*g/L)), 
       color = "", 
       shape = "", 
       title = "Port Susan buoy") + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b")
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_chlorophyll_comparison.png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - chlorophyll, single year ####
yoi <- 2024

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Chlorophyll", 
         year(DateTime) == yoi)

data_to_plot2 <- data_discrete %>% 
  filter(ParmId == 1, 
         year(CollectDate) == yoi, 
         DepthBin == "surface")

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
       y = expression(Chlorophyll~fluorescence~(mu*g/L)), 
       title = paste("Pt. Susan buoy", yoi)) + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  scale_y_continuous(limits = c(0, 100))
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_chlorophyll.png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - oxygen, all years ####
yoi <- 2024

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
  geom_point() + 
  scale_y_continuous(limits = c(0, NA))
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_DO_comparison.png")), 
       dpi = 600, height = 4, width = 6)

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
yoi <- 2024

data_to_plot <- data_buoy_new %>% 
  filter(year(DateTime) == yoi)

ggplot(data_to_plot, 
       aes(x = DateTime, 
           y = NO23)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "", 
       y = expression("NO"[2]^"-"~"+"~"NO"[3]^"-"~"(mg N/L)")) + 
  scale_y_continuous(limits = c(0, NA))
ggsave(here("figs", "psusanbuoy", 
            paste0(yoi, "_no23.png")), 
       dpi = 600, height = 6, width = 4)

#### Figure - daily mean temperature ####
yoi <- 2024

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





#### Figure - salinity + river flow ####
# Summarize salinity - daily
data_subset <- data_buoy_qc %>% 
  filter(Type == "KC", 
         Parameter == "Salinity", 
         Flag != 4) %>% 
  mutate(Date = as.Date(
    str_sub(as.character(DateTime), start = 1, end = 10))) %>% 
  group_by(Date) %>% 
  summarize(Mean = mean(Value), 
            Max = max(Value), 
            Min = min(Value)) %>% 
  mutate(Range = Max - Min)

# Summarize rivers 
data_river_subset <- data_river %>% 
  rename(RealDate = Date, 
         Flow24 = Flow) %>% 
  arrange(RealDate) %>% 
  mutate(Date = RealDate + 1, 
         Flow48 = rollsumr(Flow24, k = 2, fill = NA), 
         Flow72 = rollsumr(Flow24, k = 3, fill = NA), 
         Flowweek = rollsumr(Flow24, k = 7, fill = NA)) %>% 
  select(-RealDate)

# Combine datasets 
data_subset <- full_join(data_subset, data_river_subset) %>% 
  filter(Date >= as.Date("2022-02-03"))
  
# Make a figure
ggplot(data = data_subset, 
       aes(x = Date)) + 
  theme_bw() + 
  geom_line(aes(y = Mean, 
                color = Flow72)) + 
  scale_color_distiller(palette = "YlGnBu", transform = "log10", direction = 1)

#### Figure - NNN + chl + river flow ####
# Summarize - daily
data_subset <- data_buoy_new %>% 
  mutate(
    NO23 = ifelse(
      between(NO23, 0, 0.6), NO23, NA
    ), 
    Chlorophyll = ifelse(
      between(Chlorophyll, 0, 100), Chlorophyll, NA)
    ) %>% 
  group_by(Date) %>% 
  summarize(MeanN = mean(NO23, na.rm = TRUE), 
            MaxN = max(NO23, na.rm = TRUE), 
            MinN = min(NO23, na.rm = TRUE), 
            MeanChl = mean(Chlorophyll, na.rm = TRUE), 
            MaxChl = max(Chlorophyll, na.rm = TRUE), 
            MinChl = min(Chlorophyll, na.rm = TRUE), 
            N = n()) %>% 
  filter(N >= 48) %>% 
  mutate(RangeN = MaxN - MinN, 
         RangeChl = MaxChl - MinChl)

# Summarize rivers 
data_river_subset <- data_river %>% 
  rename(RealDate = Date, 
         Flow24 = Flow) %>% 
  arrange(RealDate) %>% 
  mutate(Date = RealDate + 1, 
         Flow48 = rollsumr(Flow24, k = 2, fill = NA), 
         Flow72 = rollsumr(Flow24, k = 3, fill = NA), 
         Flowweek = rollsumr(Flow24, k = 7, fill = NA)) %>% 
  select(-RealDate)

# Combine datasets 
data_subset <- full_join(data_subset, data_river_subset) %>% 
  filter(Date >= as.Date("2023-12-20"), 
         Date < as.Date("2025-01-01")) %>% 
  mutate(Hilite = Date %in% as.Date(
    c("2024-02-25", "2024-03-28", "2024-04-07", "2024-08-05", "2024-08-14", "2024-09-08")))

# Make a figure
ggplot(data = data_subset, 
             aes(x = Date)) + 
  theme_bw() + 
  geom_ribbon(aes(ymin = 0, ymax = Flow72/40000), fill = "grey") + 
  geom_line(aes(y = MeanN, 
                color = MeanChl), 
            size = 1) + 
  scale_color_distiller(trans = "log10", 
                        palette = "Greens", 
                        direction = 1) + 
  geom_segment(aes(xend = Date, 
                   y = MeanN - 0.03,
                   yend = MeanN - 0.01, 
                   linetype = Hilite), 
               arrow = arrow(length = unit(0.1, "cm"))) + 
  scale_linetype_manual(values = c(NA, "solid"), 
                        guide = "none") + 
  scale_x_date(expand = c(0, 0), 
               date_breaks = "3 months", 
               date_minor_breaks = "1 month", 
               date_labels = "%b %Y") + 
  scale_y_continuous(expand = c(0, 0), 
                     sec.axis = sec_axis(trans=~.*40000, 
                                         name="Stillaguamish River discharge (cfs)")) + 
  labs(x = "", 
       y = "Nitrate + nitrite N (mg/L)", 
       color = "log(Chl)")
ggsave(here("figs", "psusanbuoy", "N_chl_river.png"), 
       dpi = 600, height = 3, width = 6)

p1 <- ggplot(data = data_subset, 
             aes(x = Date)) + 
  theme_bw() + 
  geom_ribbon(aes(ymin = 0, ymax = Flow72/40000), fill = "grey") + 
  geom_line(aes(y = MeanN)) + 
  labs(x = "", y = "", title = "Nitrate + nitrite N (mg/L)") + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
p2 <- ggplot(data = data_subset, 
             aes(x = Date)) + 
  theme_bw() + 
  geom_ribbon(aes(ymin = 0, ymax = Flow72/1000), fill = "grey") + 
  geom_line(aes(y = MeanChl)) + 
  labs(x = "", y = "", title = "Chlorophyll fluorescence (ug/L)") + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))

library(patchwork)
p1/p2
ggsave(here("figs", "psusanbuoy", "N_chl_river_multipanel.png"), 
       dpi = 600, height = 6, width = 6)
