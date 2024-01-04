#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(waterData)
library(rtide)

data_buoy <- load_PS_buoy_old() %>% 
  mutate(Type = "KC") %>% 
  select(-Turbidity, 
         -TurbiditySD, 
         -ChlorophyllSD, 
         -EventFlags, 
         -Pressure)
data_buoy_ST <- load_PS_buoy_ST() %>% 
  mutate(Type = "ST")
data_buoy_comb <- full_join(data_buoy, data_buoy_ST)
data_buoy_avg <- data_buoy_comb %>% 
  group_by(Date) %>% 
  summarize()
data_discrete <- load_whidbey_discrete() %>% 
  mutate(Type = "bottle", 
         DateTime = CollectDateTime) %>% 
  filter(Locator == "PSUSANBUOY")

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
                         year(DateTime) == 2022, 
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

#### Chlorophyll ####
yoi <- 2022

data_to_plot <- data_buoy_qc %>% 
  filter(Parameter == "Chlorophyll", 
         year(DateTime) <= yoi, 
         !(Flag %in% 3:4)) %>% 
  mutate(YearGroup = ifelse(year(DateTime) == yoi, 
                            yoi, 
                            paste(min(year(DateTime)), 
                                  yoi-1, 
                                  sep = "-")), 
         FakeDate = DateTime)
year(data_to_plot$FakeDate) <- yoi

ggplot(data_to_plot, 
       aes(x = FakeDate, 
           y = Value, 
           color = YearGroup, 
           shape = Type)) + 
  geom_point()

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
