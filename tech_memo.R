#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(waterData)

good_quals_ctd <- c(NA, "TA")
good_quals_discrete <- 0:2

#### LOAD DATA ####
data_ctd <- load_composite(0.5, monthly = FALSE) %>% 
  mutate(Basin = "Whidbey")
data_central <- load_composite(0.5, location = "Central", monthly = FALSE) %>% 
  mutate(Basin = "Central")
data_discrete <- load_whidbey_discrete()
data_coupeville <- load_coupeville()
data_penncovesurf <- load_penncovesurf() %>% 
  rename_with(~str_c(., "_surface"), .cols = Temperature:NO23)
data_penncovebottom <- load_penncovebottom() %>% 
  rename_with(~str_c(., "_bottom"), .cols = HCEP_id:NO3_n)
data_penncove <- full_join(data_penncovesurf, data_penncovebottom)
data_psusan <- load_psusan()
data_river <- importDVs("12167000", 
                        sdate = "2022-01-01") %>% 
  mutate(River = "Stillaguamish") %>% 
  rename(Date = dates, 
         Flow = val) %>% 
  select(Date, Flow)

#### Figure - Penn Cove mooring DO ####
data_to_plot <- data_penncove %>% 
  select(DateTime, Oxygen_surface, Oxygen_mgL_bottom) %>% 
  rename(Surface = Oxygen_surface, 
         Bottom = Oxygen_mgL_bottom) %>% 
  pivot_longer(cols = Surface:Bottom, 
               names_to = "Depth", 
               values_to = "Oxygen") %>% 
  mutate(Oxygen = ifelse(
    between(Oxygen, 0, 20), 
    Oxygen, 
    NA
  )) %>% 
  mutate(Oxygen = case_when(
    Depth == "Bottom" & 
      DateTime == as.POSIXct("2023-12-20 14:15:00", tz = "Etc/GMT+8") ~ NA, 
    Depth == "Surface" & 
      DateTime %in% c(
        as.POSIXct("2024-02-13 10:30:00", tz = "Etc/GMT+8")) ~ NA, 
    Depth == "Surface" & 
      between(DateTime, 
              as.POSIXct("2024-04-23 09:45", tz = "Etc/GMT+8"), 
              as.POSIXct("2024-04-23 14:15", tz = "Etc/GMT+8")) ~ NA, 
    TRUE ~ Oxygen))
data_summary <- data_to_plot %>% 
  filter(Depth == "Bottom") %>% 
  mutate(IsHypoxic = Oxygen <= 2) %>% 
  summarize(PercHypoxic = sum(IsHypoxic, na.rm = TRUE) / n())
ggplot(data_to_plot, 
       aes(x = DateTime, y = Oxygen, color = Depth)) + 
  geom_line() + 
  theme_bw() + 
  labs(x = "", y = "Oxygen (mg/L)", color = "") + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %n %Y") + 
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 6, linetype = "dashed", color = "black")
ggsave(here("figs", "tech_memo_penncove_do.png"), 
       dpi = 600, height = 3, width = 7)
  
#### Figure - Coupeville DO ####
data_to_plot <- data_coupeville %>% 
  mutate(Oxygen = ifelse(Oxygen < 0, NA, Oxygen))
ggplot(
  data_to_plot, 
  aes(x = DateTime, y = Oxygen)
) + 
  theme_bw() + 
  geom_line() + 
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %n %Y") + 
  labs(x = "", 
       y = "Oxygen (mg/L)", 
       title = "Coupeville")

#### Figure - Port Susan DO ####
data_to_plot <- data_psusan %>% 
  mutate(Oxygen = case_when(
    Oxygen < 0 ~ NA, 
    between(DateTime, 
           as.POSIXct("2024-08-11 09:15", tz = "Etc/GMT+8"), 
           as.POSIXct("2024-08-14 09:15", tz = "Etc/GMT+8")) ~ NA, 
    DateTime %in% c(
          as.POSIXct("2024-07-28 16:45", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-25 13:45", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-23 01:30", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-21 15:15", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-21 08:45", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-20 13:45", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-19 11:45", tz = "Etc/GMT+8"), 
          as.POSIXct("2024-07-19 05:45", tz = "Etc/GMT+8")) ~ NA,
      TRUE ~ Oxygen
    )
  )

ggplot(
  data_to_plot, 
  aes(x = DateTime, y = Oxygen)
) + 
  theme_bw() + 
  geom_line() + 
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %n %Y") + 
  labs(x = "", 
       y = "Oxygen (mg/L)", 
       title = "Port Susan buoy")

#### Figure - Penn Cove mooring N ####
data_to_plot <- data_penncove %>% 
  select(DateTime, NO23_surface, NO3_mgNL_bottom) %>% 
  rename(Surface = NO23_surface, 
         Bottom = NO3_mgNL_bottom) %>% 
  pivot_longer(cols = Surface:Bottom, 
               names_to = "Depth", 
               values_to = "NO23") %>% 
  mutate(
    NO23 = case_when(
      NO23 < 0 ~ NA, 
      NO23 > 1 ~ NA, 
      # Depth == "Surface" & 
      #   DateTime == as.POSIXct("2024-04-21 20:45", tz = "") ~ NA, 
      TRUE ~ NO23), 
    Type = "mooring")

discrete_to_plot <- data_discrete %>% 
  filter(ParmId == 14, 
         Locator == "PENNCOVEENT", 
         DepthBin %in% c("25 m", "surface")) %>% 
  mutate(Type = "bottle", 
         Depth = ifelse(DepthBin == "surface", 
                        "Surface", 
                        "Bottom"), 
         DateTime = CollectDateTime, 
         NO23 = Value) %>% 
  select(DateTime, Type, Depth, NO23, Detect)
combo_data <- full_join(data_to_plot, discrete_to_plot)
ggplot() + 
  geom_line(
    data = data_to_plot, 
    aes(x = DateTime, y = NO23, color = Depth)
  ) + 
  scale_color_brewer(palette = "Paired") + 
  geom_point(
    data = discrete_to_plot, 
    aes(x = DateTime, y = NO23, shape = Detect)
  ) + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
  facet_wrap(~Depth, ncol = 1) + 
  theme_bw() + 
  labs(x = "", y = "Nitrate+nitrite N (mg/L)", color = "") + 
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %n %Y")

#### Figure - Coupeville N ####
data_to_plot <- data_coupeville %>% 
  mutate(NO23 = case_when(
    NO23 < 0 ~ NA, 
    NO23 > 0.5 ~ NA, 
    TRUE ~ NO23
  ))
ggplot(
  data_to_plot, 
  aes(x = DateTime, y = NO23)
) + 
  theme_bw() + 
  geom_line() + 
  scale_x_datetime(date_breaks = "2 months", 
                   date_labels = "%b %n %Y") + 
  labs(x = "", 
       y = "Nitrate + nitrite N (mg/L)", 
       title = "Coupeville")

#### Figure - Port Susan N+S w/ river ####
data_to_plot <- data_psusan %>% 
  mutate(NO23 = case_when(
    NO23 < 0 ~ NA, 
    TRUE ~ NO23
    )
  ) %>% 
  group_by(Date) %>% 
  summarize(MeanN = mean(NO23, na.rm = TRUE), 
            MeanS = mean(Salinity, na.rm = TRUE)) %>% 
  left_join(data_river) %>% 
  pivot_longer(c(MeanN, MeanS, Flow), 
               names_to = "Parameter", 
               values_to = "Value")

ggplot(
  data_to_plot %>% filter(Parameter != "MeanS"), 
  aes(x = Date, y = Value)
) + 
  theme_bw() + 
  geom_line() + 
  facet_wrap(~ Parameter, 
             ncol = 1, 
             scales = "free_y", 
             strip.position = "left", 
             labeller = as_labeller(
               c(Flow = "Stillaguamish R \n flow (cfs)", 
                 MeanN = "Nitrate+nitrite \n N (mg/L)"))) + 
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%b %n %Y") + 
  theme(strip.background = element_blank(),
        strip.placement = "outside") + 
  labs(x = "", 
       y = "", 
       title = "Port Susan buoy")
ggsave(here("figs", "tech_memo_psusanbuoy_N_river.png"), 
       dpi = 600, height = 3, width = 7)

ggplot(
  data_to_plot, 
  aes(x = Date)
) + 
  theme_bw() + 
  geom_line(aes(y = MeanS)) + 
  geom_line(aes(y = Flow/1000), color = "blue") + 
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%b %n %Y") + 
  labs(x = "", 
       y = "Nitrate + nitrite N (mg/L)", 
       title = "Port Susan buoy")

#### Figure - all stations minimum DO (CTD) ####
yoi <- 2024

data_to_plot <- data_ctd %>% 
  filter(Locator != "PENNCOVEPNN001", 
         Year >= 2020) %>% 
  group_by(Locator, Date) %>% 
  summarize(MinDO = min(DO)) %>% 
  mutate(FakeDate = Date)
year(data_to_plot$FakeDate) <- 2020 

ggplot(data = data_to_plot, 
       aes(x = FakeDate, 
           y = MinDO, 
           color = year(Date) == yoi, 
           text = year(Date))) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point(size = 3) + 
  facet_wrap(~ Locator, 
             ncol = 5) + 
  labs(x = "", 
       y = "Minimum DO (mg/L)", 
       color = "") + 
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b") + 
  scale_color_manual(values = c("gray", "black")) + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4)) + 
  geom_hline(yintercept = 6,  
             linetype = "dashed", 
             color = "black") + 
  geom_hline(yintercept = 2, 
             linetype = "dashed", 
             color = "red")

ggsave(here("figs", "tech_memo_min_DO.png"), 
       dpi = 600, height = 4, width = 7)

#### Figure - Port Susan weird bottom DO/T/S ####
data_to_plot <- data_ctd %>% 
  filter(Locator == "PSUSANENT", Month == 4) %>% 
  select(Year, Date, Depth, DO, Salinity, Temperature) %>% 
  pivot_longer(DO:Temperature, 
               names_to = "Parameter", 
               values_to = "Value") %>% 
  arrange(Depth)

ggplot(data_to_plot, 
       aes(x = Depth, 
           y = Value, 
           group = Date, 
           color = as.factor(Year))) + 
  theme_bw() + 
  scale_color_viridis_d(end = 0.9, direction = -1) + 
  geom_path() + 
  facet_wrap(~Parameter, 
             scales = "free_x", 
             labeller = as_labeller(
               c(DO = "Oxygen (mg/L)", 
                 Salinity = "Salinity (PSU)", 
                 Temperature = "Temperature (deg C)"))) + 
  coord_flip() + 
  scale_x_reverse() + 
  labs(y = "", x = "Depth (m)", color = "")

ggsave(here("figs", "tech_memo_psusan_ctd.png"), 
       dpi = 600, height = 3, width = 7)

#### Figure - integrated chlorophyll ####
totalchl <- data_ctd %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         !is.na(Chlorophyll)) %>% 
  group_by(Locator, Date) %>% 
  summarize(Int_chl = pracma::trapz(Depth, Chlorophyll)) %>% 
  mutate(FakeDate = Date)
year(totalchl$FakeDate) <- 2024

ggplot(data = totalchl %>% 
         filter(year(Date) >= 2022), 
       aes(x = FakeDate, 
           y = Int_chl, 
           color = as.factor(year(Date)))) + 
  facet_wrap(~year(Date), 
             ncol = 1, 
             scales = "free_y") + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(se = FALSE, size = 2) + 
  theme(legend.position = "none") + 
  labs(x = "", 
       y = expression(Chlorophyll~fluorescence~(mg/m^2)), 
       color = "") + 
  scale_color_viridis_d(end = 0.9, direction = -1) + 
  lims(y = c(0, NA)) + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")
ggsave(here("figs", "tech_memo_intchl.png"), 
       dpi = 600, height = 5, width = 5)
