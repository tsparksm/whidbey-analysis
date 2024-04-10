#### SETUP ####
source(here::here("src", "utility_functions.R"))

# Load in raw Port Susan buoy output downloaded from Hydrosphere
# fpath is an optional filepath specification; if blank will pop up file selection
load_hydrosphere_psusan <- function(fpath) {
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
                             `WTX534(Pressure)` = col_double()
                           ))
}

# Take raw hydrosphere output from Port Susan buoy, filter by datetime, and save .csv
# .csv is in the correct format to upload/append to Socrata
# Date format should be "YYYY-MM-DD"
# Time format should be "HH:MM" in 24 hr clock
process_socrata_psusan <- function(start_date, 
                                   start_time) {
  raw_data <- load_hydrosphere_psusan() %>% 
    filter(`Date(America/Los_Angeles)` > start_date | 
             `Date(America/Los_Angeles)` == start_date & 
             `Time(America/Los_Angeles)` >= as_hms(paste0(start_time, ":00")))
  
  fpath <- here("data", "socrata", paste0("psusan_socrata_", 
                                          Sys.Date(), 
                                          ".csv"))
  
  write_csv(raw_data, 
            file = fpath)
}

#### Routine Socrata fill ####
start_dt <- dlg_input("Start datetime? (YYYY-MM-DD HH:MM)")$res
start_time <- str_sub(start_dt, 12, 16)
start_date <- str_sub(start_dt, 1, 10)

process_socrata_psusan(start_date, start_time)

#### Initial Hydrosphere download #### 
