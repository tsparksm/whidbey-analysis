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
                             Date = col_date(), 
                             Time = col_time(), 
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
                             `WTX534(Pressure)` = col_double()
                           ))
}

# Take raw hydrosphere output from Penn Cove surface, filter by datetime, and save .csv
# .csv is in the correct format to upload/append to Socrata
# Date format should be "YYYY-MM-DD"
# Time format should be "HH:MM" in 24 hr clock
process_socrata_penncovesurf <- function(start_date, 
                                         start_time, 
                                         end_date, 
                                         end_time) {
  raw_data <- load_hydrosphere_penncovesurf() %>% 
    mutate(DateTime = as.POSIXct(paste(Date, Time), 
                                 tz = "UTC"), 
           NewDateTime = with_tz(DateTime, tzone = "Etc/GMT+8"), 
           Date = as.Date(str_sub(NewDateTime, 1, 10)), 
           Time = parse_time(str_sub(NewDateTime, 12, 16)), 
           `SDI-12Sensor(M_Parameter1)(Inactive-1)` = NA, 
           `SDI-12Sensor(M_Parameter2)(Inactive-2)` = NA, 
           `SDI-12Sensor(M_Parameter3)(Inactive-3)` = NA) %>% 
    select(-DateTime, -NewDateTime) %>% 
    filter(Date > start_date | 
             Date == start_date & 
             Time >= as_hms(paste0(start_time, ":00")))
  
  if (!is.na(end_date)) {
    raw_data <- raw_data %>% 
      filter(Date < end_date | 
               Date == end_date & 
               Time <= as_hms(paste0(end_time, ":00")))
  } else {
    end_date = max(raw_data$Date)
  }
  
  fpath <- here("data", "socrata", paste0("penncovesurf_socrata_", 
                                          start_date, "_",
                                          end_date,
                                          ".csv"))
  
  write_csv(raw_data, 
            file = fpath)
}

#### Routine Socrata fill ####
start_dt <- dlg_input("Start datetime? (YYYY-MM-DD HH:MM)")$res
start_time <- str_sub(start_dt, 12, 16)
start_date <- str_sub(start_dt, 1, 10)

end_dt <- dlg_input("(Optional) end datetime? (YYYY-MM-DD HH:MM) Cancel if none", default = "")$res
if (is_empty(end_dt)) {
  end_time <- NA
  end_date <- NA
} else {
  end_time <- str_sub(end_dt, 12, 16)
  end_date <- str_sub(end_dt, 1, 10)
}

process_socrata_penncovesurf(start_date, start_time, end_date, end_time)

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
           between(rn, 3657, 15749) ~ DateTime + 1*60*60, 
           TRUE ~ DateTime), 
         Date = str_sub(as.character(NewTime), 1, 10), 
         Time = str_sub(as.character(NewTime), 12, 16)
  ) %>% 
  filter(rn >= 34) %>% 
  select(-NewTime, -DateTime, -rn)

fpath <- here("data", "socrata", "penncovesurf_socrata_initial.csv")

write_csv(processed_data, file = fpath)
