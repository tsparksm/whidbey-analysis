#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)
library(plotly)
library(zoo)
library(readxl)

#### Load data ####
# Remember to update file via Socrata download first!
raw_data <- load_penncovebottom() %>% 
  select(-c(Conductivity_Sm, 
            Chlorophyll_SD_ugL, 
            Turbidity_SD_ugL, 
            Spec_Conductivity_Sm, 
            NO3_umol, 
            NO3_n)) %>% 
  relocate(c(Event_Flags, SUNA_id), .after = HCEP_id) %>% 
  rename(Temperature = Temperature_C, 
         Pressure = Pressure_dbar, 
         Oxygen = Oxygen_mgL, 
         OxygenSat = Oxygen_sat, 
         Chlorophyll = Chlorophyll_ugL, 
         Turbidity = Turbidity_NTU, 
         Salinity = Salinity_PSU, 
         NO23 = NO3_mgNL) %>% 
  mutate(Date = as.Date(DateTime))

# Remember to update discrete data first!
# download_whidbey_discrete()
bottle_data <- load_whidbey_discrete() %>% 
  filter(Locator == "PENNCOVEENT", 
         Depth > 20)

ctd_data <- load_CTD("PENNCOVEENT")

#### QC auto - gross + site range ####
qc_data <- raw_data %>% 
  mutate(
    Temperature_auto = case_when(
      !between(Temperature, -5, 45) ~ "g", 
      !between(Temperature, 9, 13) ~ "r", 
      TRUE ~ NA
    ), 
    Pressure_auto = case_when(
      Pressure < 0 ~ "g", 
      !between(Pressure, 23, 29) ~ "r", 
      TRUE ~ NA
    ), 
    Oxygen_auto = case_when(
      !between(Oxygen, 0, 25) ~ "g", 
      Oxygen > 8 ~ "r", 
      TRUE ~ NA
    ), 
    pH_auto = case_when(
      !between(pH, 0, 14) ~ "g", 
      !between(pH, 6, 8) ~ "r", 
      TRUE ~ NA
    ), 
    Chlorophyll_auto = case_when(
      !between(Chlorophyll, 0, 400) ~ "g", 
      Chlorophyll > 20 ~ "r", 
      TRUE ~ NA
    ), 
    Turbidity_auto = case_when(
      !between(Turbidity, 0, 3000) ~ "g", 
      Turbidity > 400 ~ "r", 
      TRUE ~ NA
    ), 
    Salinity_auto = case_when(
      !between(Salinity, 0, 42) ~ "g", 
      !between(Salinity, 27, 31) ~ "r", 
      TRUE ~ NA
    ), 
    NO23_auto = case_when(
      !between(NO23, 0, 56) ~ "g", 
      !between(NO23, 0.14, 0.5) ~ "r", 
      TRUE ~ NA
    ), 
    DateTime = round_date(DateTime, "15 mins"), 
    FakeDateTime = round_date(FakeDateTime, "15 mins")
  )

#### QC manual - load/apply spreadsheet of flagged values ####
manual_qc <- read_xlsx(
  here("data", "whidbey mooring qc.xlsx"), 
  sheet = "Penn Cove bottom"
) %>% 
  mutate(End = if_else(is.na(End), Start, End), 
         Span = paste(Start, End))

all_dts <- manual_qc %>% 
  select(Start, End, Span) %>% 
  group_by(Span) %>% 
  group_modify(~ tibble(DateTime = seq(.$Start, .$End, by = "15 min")))

all_manual <- full_join(manual_qc, all_dts) %>% 
  select(DateTime, Pressure:NO23) %>% 
  mutate(
    DateTime = force_tz(DateTime, "Etc/GMT+8"), 
    across(
      Pressure:NO23, 
      ~ case_when(
        str_detect(.x, "q") ~ "s", 
        str_detect(.x, "endcheck") ~ "f", 
        !is.na(.x) ~ "b", 
        TRUE ~ NA
      )
    )
  ) %>% 
  group_by(DateTime) %>% 
  mutate(across(Pressure:NO23, ~ replace_na(as.character(.x), ""))) %>% 
  summarize(across(Pressure:NO23, ~ paste(.x, sep = ",", collapse = ""))) %>% 
  rename_with(~str_c(., "_manual"), .cols = Pressure:NO23)

qc_data <- left_join(qc_data, all_manual)

#### QC final - determine final quality and save file ####
# Make sure to include osat quality as a function of o + t quality
qc_final <- qc_data %>% 
  relocate(OxygenSat, .after = NO23) %>% 
  mutate(
    across(
      Temperature:NO23, 
      ~ case_when(
        pick(paste0(cur_column(), "_auto")) == "g" ~ 3, 
        pick(paste0(cur_column(), "_manual")) == "b" ~ 3,
        pick(paste0(cur_column(), "_auto")) == "r" ~ 2,
        pick(paste0(cur_column(), "_manual")) == "f" ~ 2,
        pick(paste0(cur_column(), "_manual")) == "s" ~ 2,
        TRUE ~ 1
      ), 
      .names = "{.col}_final"
    ), 
    OxygenSat_final = pmax(Oxygen_final, Temperature_final)
  )

write_csv(qc_final, 
          here("data", "penncovebottom_qc.csv"))


qc_sw <- qc_final %>% 
  select(-FakeDateTime) %>% 
  filter(Year <= 2024)
write_csv(qc_sw, 
          here("data", "penncovebottom_sw_qc.csv"))

#### FIGURE - pressure ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date))

p <- ggplot(data_to_plot, 
            aes(x = DateTime)) + 
  geom_point(aes(y = Pressure, 
                 color = as.factor(Pressure_final)), 
             size = 0.2) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "", y = "Pressure (db) / Tide height (m)")
ggplotly(p)

#### FIGURE - temperature ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date)) %>% 
  mutate(Quality = as.factor(Temperature_final))

bottle_data_tp <- bottle_data %>% 
  filter(between(CollectDate, start_date, end_date), 
         ParmId == 22)

p <- ggplot(data_to_plot) + 
  geom_point(size = 0.2, 
             aes(x = DateTime, 
                 y = Temperature, 
                 color = Quality)) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  labs(x = "", 
       y = "Temperature (\u00B0C)") + 
  theme_bw() + 
  theme(legend.position = "none") + 
  geom_point(data = bottle_data_tp, 
             aes(x = CollectDateTime, 
                 y = Value), 
             color = "blue", 
             shape = 8)
ggplotly(p)

#### FIGURE - salinity ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date)) %>% 
  mutate(Quality = as.factor(Salinity_final))

bottle_data_tp <- bottle_data %>% 
  filter(between(CollectDate, start_date, end_date), 
         ParmId %in% 18:19) %>% 
  mutate(Type = ifelse(ParmId == 18, "Lab", "Field"))

p <- ggplot(data_to_plot) + 
  geom_point(aes(x = DateTime, 
                 y = Salinity, 
                 color = Quality), 
             size = 0.2) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  labs(x = "", 
       y = "Salinity (PSU)", 
       shape = "") + 
  geom_point(data = bottle_data_tp, 
             aes(x = CollectDateTime, 
                 y = Value, 
                 shape = Type), 
             color = "blue") + 
  scale_shape_manual(values = c("Lab" = 2, "Field" = 8)) + 
  guides(color = "none")
ggplotly(p) %>% layout(legend = list(orientation = "h"))

#### FIGURE - oxygen ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date)) %>% 
  mutate(Quality = as.factor(Oxygen_final))

bottle_data_tp <- bottle_data %>% 
  filter(between(CollectDate, start_date, end_date), 
         ParmId %in% 5:6) %>% 
  mutate(Type = ifelse(ParmId == 5, "Lab", "Field"))

p <- ggplot(data_to_plot) + 
  geom_point(aes(x = DateTime, 
                 y = Oxygen, 
                 color = Quality), 
             size = 0.2) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "", y = "Oxygen (mg/L)", shape = "") + 
  geom_point(data = bottle_data_tp, 
             aes(x = CollectDateTime, 
                 y = Value, 
                 shape = Type), 
             color = "blue") + 
  scale_shape_manual(values = c("Lab" = 2, "Field" = 8)) + 
  guides(color = "none")
ggplotly(p) %>% layout(legend = list(orientation = "h"))

#### FIGURE - pH ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date))

p <- ggplot(data_to_plot, 
            aes(x = DateTime, 
                y = pH, 
                color = as.factor(pH_final))) + 
  geom_point(size = 0.2) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "", y = "pH")
ggplotly(p)

#### FIGURE - chlorophyll ####
start_date <- as.Date("2024-05-01")
end_date <- as.Date("2024-07-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date)) %>% 
  mutate(Quality = as.factor(Chlorophyll_final))

bottle_data_tp <- bottle_data %>% 
  filter(between(CollectDate, start_date, end_date), 
         ParmId %in% 1:2) %>% 
  mutate(Type = ifelse(ParmId == 1, "Lab", "Field"))

p <- ggplot(data_to_plot) + 
  geom_point(size = 0.2, 
             aes(x = DateTime, 
                 y = Chlorophyll, 
                 color = Quality)) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  labs(x = "", 
       y = "Chlorophyll (ug/L)", 
       shape = "") + 
  geom_point(data = bottle_data_tp, 
             aes(x = CollectDateTime, 
                 y = Value, 
                 shape = Type), 
             color = "blue") + 
  scale_shape_manual(values = c("Lab" = 2, "Field" = 8)) + 
  guides(color = "none")
ggplotly(p) %>% layout(legend = list(orientation = "h"))

#### FIGURE - turbidity ####
start_date <- as.Date("2024-04-01")
end_date <- as.Date("2024-09-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date))

p <- ggplot(data_to_plot, 
            aes(x = DateTime, 
                y = Turbidity, 
                color = as.factor(Turbidity_final))) + 
  geom_point(size = 0.2) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "", 
       y = "Turbidity (NTU)") + 
  lims(y = c(0, NA))
ggplotly(p)

#### FIGURE - NO23 ####
start_date <- as.Date("2024-04-01")
end_date <- as.Date("2024-05-01")

data_to_plot <- qc_final %>% 
  filter(between(Date, start_date, end_date)) %>% 
  mutate(Quality = as.factor(NO23_final))

bottle_data_tp <- bottle_data %>% 
  filter(between(CollectDate, start_date, end_date), 
         ParmId == 14) 

p <- ggplot(data_to_plot) + 
  geom_point(size = 0.2, 
             aes(x = DateTime, 
                 y = NO23, 
                 color = Quality)) + 
  scale_color_manual(values = c("1" = "black", 
                                "2" = "orange", 
                                "3" = "red")) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(x = "", y = "NO23 (mg N/L)") + 
  geom_point(data = bottle_data_tp, 
             aes(x = CollectDateTime, 
                 y = Value), 
             color = "blue", 
             shape = 8)
ggplotly(p)

