#### SETUP ####
source(here::here("src", "utility_functions.R"))

# Load in raw Penn Cove surface mooring output downloaded from Hydrosphere
# fpath is an optional filepath specification; if blank will pop up file selection
load_hydrosphere_penncovesurf <- function(fpath) {
  if (missing(fpath)) {
    fpath <- choose.files(caption = "Select file to load", 
                          multi = FALSE)
  }
  
  mooring_data <- read_csv(fpath, 
                           col_types = cols(
                             UnixTimestamp = col_double(), 
                             `Date(America/Los_Angeles)` = col_date(
                               format = "%m/%d/%Y"), 
                             `Time(America/Los_Angeles)` = col_time(), 
                             WindDirection = col_double(), 
                             WindSpeed = col_double(), 
                             SystemBattery = col_double(), 
                             `HCEP(TEMP)` = col_double(), 
                             `HCEP(COND)` = col_double(), 
                             `HCEP(PRES)` = col_double(), 
                             `HCEP(OXY)` = col_double(), 
                             `HCEP(PH)` = col_double(), 
                             `HCEP(FL)` = col_double(), 
                             `HCEP(TURB)` = col_double(), 
                             `HCEP(FSD)` = col_double(), 
                             `HCEP(TSD)` = col_double(), 
                             `HCEP(SAL)` = col_double(), 
                             `HCEP(SNDV)` = col_double(), 
                             `HCEP(SPC)` = col_double(), 
                             `HCEP(OSAT)` = col_double(), 
                             `HCEP(VOLT)` = col_double(), 
                             `HCEP(NUM)` = col_double(), 
                             `SUNA(M_Parameter1)` = col_double(), 
                             `SUNA(M_Parameter2)` = col_double(), 
                             `SUNA(M_Parameter3)` = col_double(), 
                             `SUNA(M_Parameter4)` = col_double(), 
                             `HCEP(ERR)` = col_double(), 
                             `PAR(PAR_SQ-421X)` = col_double(), 
                             `WTX534(Temp)` = col_double(), 
                             `WTX534(Humidity)` = col_double(), 
                             `WTX534(Pressure)` = col_double(), 
                             `SDI-12Sensor(M_Parameter1)` = col_double(), 
                             `SDI-12Sensor(M_Parameter2)` = col_double(), 
                             `SDI-12Sensor(M_Parameter3)` = col_double()
                           ))
}

# Take raw hydrosphere output from Penn Cove surface, filter by datetime, and save .csv
# .csv is in the correct format to upload/append to Socrata
# Date format should be "YYYY-MM-DD"
# Time format should be "HH:MM" in 24 hr clock
process_socrata_penncovesurf <- function(start_date, 
                                         start_time) {
  raw_data <- load_hydrosphere_penncovesurf() %>% 
    filter(`Date(America/Los_Angeles)` > start_date | 
             `Date(America/Los_Angeles)` == start_date & 
             `Time(America/Los_Angeles)` >= as_hms(paste0(start_time, ":00"))) %>% 
    rowwise() %>% 
    mutate(`SDI-12Sensor(M_Parameter1)` = mean(c(`SDI-12Sensor(M_Parameter1)`, 
                                                 `WTX534(Temp)`), 
                                               na.rm = TRUE), 
           `SDI-12Sensor(M_Parameter2)` = mean(c(`SDI-12Sensor(M_Parameter2)`, 
                                                 `WTX534(Humidity)`), 
                                               na.rm = TRUE), 
           `SDI-12Sensor(M_Parameter3)` = mean(c(`SDI-12Sensor(M_Parameter3)`, 
                                                 `WTX534(Pressure)`), 
                                               na.rm = TRUE)) %>% 
    select(-`WTX534(Temp)`, -`WTX534(Humidity)`, -`WTX534(Pressure)`)
  
  fpath <- here("data", "socrata", paste0("penncovesurf_socrata_", 
                                          Sys.Date(), 
                                          ".csv"))
  
  write_csv(raw_data, 
            file = fpath)
}

#### Routine Socrata fill ####
start_dt <- dlg_input("Start datetime? (YYYY-MM-DD HH:MM)")$res
start_time <- str_sub(start_dt, 12, 16)
start_date <- str_sub(start_dt, 1, 10)

process_socrata_penncovesurf(start_date, start_time)

#### Initial Hydrosphere download ####
fpath <- here("data", "raw", "Initial Hydrosphere download", 
              "Penn Cove Surface.csv")
mooring_data <- mooring_data <- read_csv(
  fpath, 
  col_types = cols(
    UnixTimestamp = col_double(), 
    `Date(America/Los_Angeles)` = col_date(
      format = "%m/%d/%Y"), 
    `Time(America/Los_Angeles)` = col_time(), 
    WindDirection = col_double(), 
    WindSpeed = col_double(), 
    SystemBattery = col_double(), 
    `HCEP(TEMP)` = col_double(), 
    `HCEP(COND)` = col_double(), 
    `HCEP(PRES)` = col_double(), 
    `HCEP(OXY)` = col_double(), 
    `HCEP(PH)` = col_double(), 
    `HCEP(FL)` = col_double(), 
    `HCEP(TURB)` = col_double(), 
    `HCEP(FSD)` = col_double(), 
    `HCEP(TSD)` = col_double(), 
    `HCEP(SAL)` = col_double(), 
    `HCEP(SNDV)` = col_double(), 
    `HCEP(SPC)` = col_double(), 
    `HCEP(OSAT)` = col_double(), 
    `HCEP(VOLT)` = col_double(), 
    `HCEP(NUM)` = col_double(), 
    `SUNA(M_Parameter1)` = col_double(), 
    `SUNA(M_Parameter2)` = col_double(), 
    `SUNA(M_Parameter3)` = col_double(), 
    `SUNA(M_Parameter4)` = col_double(), 
    `HCEP(ERR)` = col_double(), 
    `PAR(PAR_SQ-421X)` = col_double(), 
    `WTX534(Temp)` = col_double(), 
    `WTX534(Humidity)` = col_double(), 
    `WTX534(Pressure)` = col_double(), 
    `SDI-12Sensor(M_Parameter1)(Inactive-1)` = col_double(), 
    `SDI-12Sensor(M_Parameter2)(Inactive-2)` = col_double(), 
    `SDI-12Sensor(M_Parameter3)(Inactive-3)` = col_double()
  ))

processed_data <- mooring_data %>% 
  rename(Date = `Date(America/Los_Angeles)`, 
         Time = `Time(America/Los_Angeles)`) %>% 
  mutate(rn = row_number(), 
         DateTime = as.POSIXct(paste(Date, Time), tz = "UTC"), 
         NewTime = case_when(
           rn <= 6252 ~ DateTime - 1*60*60, 
           rn > 18345 ~ DateTime - 1*60*60, 
           TRUE ~ DateTime), 
         Date = str_sub(as.character(NewTime), 1, 10), 
         Time = str_sub(as.character(NewTime), 12, 16)
  ) %>% 
  select(-NewTime, -DateTime, -rn)

fpath <- here("data", "socrata", "coupeville_socrata_initial.csv")

write_csv(processed_data, file = fpath)
