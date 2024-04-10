#### SETUP ####
source(here::here("src", "utility_functions.R"))

#### 2023-09 to 2024-01 (actually 2023-12) ####
fpath_HCEP <- here("data", "raw", "Penn Cove near-bottom", 
                   "Penn Cove NB HCEP 2023-09 to 2024-01.xlsx")
data_HCEP <- read_xlsx(
  fpath_HCEP, 
  sheet = "Data", 
  col_types = c(
    "text", 
    "text", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "text", 
    "text", 
    "numeric"
  )
) %>% 
  rename(
    HCEP_id = FrameSync, 
    DateTime = `DateTime (UTC-07:00)`, 
    Temperature_C = `Temperature (Celsius)`, 
    Conductivity_Sm = `Conductivity (S/m)`, 
    Pressure_dbar = `Pressure (Decibar)`, 
    Oxygen_mgL = `Oxygen (mg/L)`, 
    pH = `pH (pH)`, 
    Chlorophyll_ugL = `Chlorophyll (ug/l)`, 
    Turbidity_NTU = `Turbidity (NTU)`, 
    Chlorophyll_SD_ugL = `Chlorophyll StdDev (ug/l)`, 
    Turbidity_SD_ugL = `Turbidity StdDev (NTU)`, 
    Salinity_PSU = `Salinity (psu)`, 
    Spec_Conductivity_Sm = `Spec Conductivity (S/m)`, 
    Oxygen_sat = `Oxygen Sat (%)`, 
    Event_Flags = `Event Flags`, 
    Time = `Time (UTC-07:00)`
  ) %>% 
  mutate(Date = as.Date(Date, 
                        format = "%d %b %Y"), 
         DateTime = as.POSIXct(paste(Date, Time))) %>% 
  select(-Date, -Time) %>% 
  mutate(Newtime = force_tz(DateTime, "UTC"), 
         Rowid = row_number(), 
         DateTime = if_else(
           Rowid <= 3616, 
           Newtime - 1*60*60, 
           Newtime
         )) %>% 
  select(-Rowid, -Newtime)

fpath_SUNA <- here("data", "raw", "Penn Cove near-bottom", 
                   "Penn Cove NB SUNA 2023-09 to 2024-01.xlsx")
data_SUNA <- read_xlsx(
  fpath_SUNA, 
  col_types = c(
    "text", 
    "date", 
    "date", 
    "skip", 
    "skip", 
    "numeric", 
    "numeric", 
    "skip", 
    "skip", 
    "skip", 
    "skip", 
    "skip", 
    "skip", 
    "skip"
  ), 
  skip = 7
) %>% 
  rename(
    SUNA_id = `Frame Header`, 
    NO3_umol = `Processed NO3(uMol)`, 
    NO3_mgNL = `Processed NO3(mg N/L)`
  ) %>% 
  mutate(across(NO3_umol:NO3_mgNL, 
                ~replace(., . == "NaN", NA)), 
         Time = str_sub(as.character(Time), start = 12), 
         DateTime = round_date(as.POSIXct(paste(Date, Time), 
                                          tz = "UTC"), 
                               "15 mins")) %>% 
  group_by(SUNA_id, DateTime) %>% 
  summarize(NO3_umol = mean(NO3_umol, na.rm = T), 
            NO3_mgNL = mean(NO3_mgNL, na.rm = T), 
            NO3_n = n()) %>% 
  mutate(DateTime = DateTime - 7*60*60)

data_combined <- full_join(data_HCEP, data_SUNA) %>% 
  arrange(DateTime) %>% 
  filter(row_number() <= n() - 4) %>% 
  mutate(DateTime = as.character(DateTime))

fpath <- here("data", "socrata", 
              "penncovebottom_socrata_2024-02-01.csv")
write_csv(data_combined, fpath)

#### 2023-12 to 2024-03 ####
fpath_HCEP <- here("data", "raw", "Penn Cove near-bottom", 
                   "Penn Cove NB HCEP 2023-12 to 2024-03.xlsx")
data_HCEP <- read_xlsx(
  fpath_HCEP, 
  sheet = "Data", 
  col_types = c(
    "text", 
    "text", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "numeric", 
    "text", 
    "text", 
    "numeric"
  )
) %>% 
  rename(
    HCEP_id = FrameSync, 
    DateTime = `DateTime (UTC-07:00)`, 
    Temperature_C = `Temperature (Celsius)`, 
    Conductivity_Sm = `Conductivity (S/m)`, 
    Pressure_dbar = `Pressure (Decibar)`, 
    Oxygen_mgL = `Oxygen (mg/L)`, 
    pH = `pH (pH)`, 
    Chlorophyll_ugL = `Chlorophyll (ug/l)`, 
    Turbidity_NTU = `Turbidity (NTU)`, 
    Chlorophyll_SD_ugL = `Chlorophyll StdDev (ug/l)`, 
    Turbidity_SD_ugL = `Turbidity StdDev (NTU)`, 
    Salinity_PSU = `Salinity (psu)`, 
    Spec_Conductivity_Sm = `Spec Conductivity (S/m)`, 
    Oxygen_sat = `Oxygen Sat (%)`, 
    Event_Flags = `Event Flags`, 
    Time = `Time (UTC-08:00)`
  ) %>% 
  mutate(Date = as.Date(Date, 
                        format = "%d %b %Y"), 
         DateTime = as.POSIXct(paste(Date, Time))) %>% 
  select(-Date, -Time) %>% 
  mutate(Newtime = force_tz(DateTime, "UTC"), 
         DateTime = Newtime - 8*60*60) %>% 
  select(-Newtime)

fpath_SUNA <- here("data", "raw", "Penn Cove near-bottom", 
                   "Penn Cove NB SUNA 2023-12 to 2024-03.xlsx")
data_SUNA <- read_xlsx(
  fpath_SUNA, 
  sheet = "SUNA1509_BATCH_pro", 
  col_types = c(
    "text", 
    "date", 
    "date", 
    "skip", 
    "skip", 
    "numeric", 
    "numeric", 
    "skip", 
    "skip", 
    "skip", 
    "skip", 
    "skip", 
    "skip", 
    "skip"
  ), 
  skip = 7
) %>% 
  rename(
    SUNA_id = `Frame Header`, 
    NO3_umol = `Processed NO3(uMol)`, 
    NO3_mgNL = `Processed NO3(mg N/L)`
  ) %>% 
  mutate(across(NO3_umol:NO3_mgNL, 
                ~replace(., . == "NaN", NA)), 
         Time = str_sub(as.character(Time), start = 12), 
         DateTime = round_date(as.POSIXct(paste(Date, Time), 
                                          tz = "UTC"), 
                               "15 mins")) %>% 
  group_by(SUNA_id, DateTime) %>% 
  summarize(NO3_umol = mean(NO3_umol, na.rm = T), 
            NO3_mgNL = mean(NO3_mgNL, na.rm = T), 
            NO3_n = n()) %>% 
  mutate(DateTime = DateTime - 8*60*60)

data_combined <- full_join(data_HCEP, data_SUNA) %>% 
  arrange(DateTime) %>% 
  filter(row_number() >= 7, 
         row_number() <= n() - 6) %>% 
  mutate(DateTime = as.character(DateTime))

fpath <- here("data", "socrata", 
              "penncovebottom_socrata_2024-04-09.csv")
write_csv(data_combined, fpath)



