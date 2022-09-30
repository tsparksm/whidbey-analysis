library(kcmarine)
library(tidyverse)
library(here)
library(lubridate)

# Load in Whidbey Basin CTD data
# Data must be downloaded from internal CTD website
# Ensure ALL parameters are included
load_ctd_data <- function() {
  fpath <- here("data", "raw", "whidbey_CTD.txt")
  import_CTD(fpath)
}

# Load in Whidbey Basin discrete/bottle data
# Data must be downloaded using kcmarine download_discrete()
# Adds depth bins that are relevant for Whidbey Basin sites
load_discrete_data <- function() {
  fpath <- here("data", "raw", "whidbey_discrete.csv")
  output <- import_discrete(fpath) %>% 
    mutate(Value = case_when(QfrCode == "<MDL" ~ Mdl, 
                             TRUE ~ Value),
           DepthBin = case_when(Depth < 2 ~ "surface", 
                                Depth < 7 ~ "2-6 m", 
                                Depth < 11 ~ "7-10 m", 
                                Depth < 15 ~ "11-15 m", 
                                Depth < 30 ~ "25 m", 
                                TRUE ~ "deep"), 
           DepthBin = factor(DepthBin, 
                             levels = c("surface", 
                                        "2-6 m", 
                                        "7-10 m", 
                                        "11-15 m", 
                                        "25 m", 
                                        "deep")))
}

# Load in Port Susan HCEP data (KC)
load_buoy_data <- function() {
  fpath <- here("data", "raw", "Port_Susan_buoy_HCEP.csv")
  read_csv(fpath, 
           col_types = cols(FrameSync = col_skip(), 
                            DateTime = col_datetime(format = "%m/%d/%Y %H:%M"), 
                            Temperature = col_double(), 
                            Conductivity = col_skip(), 
                            Pressure = col_double(), 
                            Oxygen = col_double(), 
                            pH = col_double(), 
                            Chlorophyll = col_double(), 
                            Turbidity = col_double(), 
                            ChlorophyllSD = col_double(), 
                            TurbiditySD = col_double(), 
                            Salinity = col_double(), 
                            SpecConductivity = col_skip(), 
                            OxygenSat = col_double(), 
                            Date = col_skip(), 
                            Time = col_skip(), 
                            EventFlags = col_double()))
}

# Load in summarized SSM output
# Output generated using external script (make_SSM_summary.R)
# Assumes scenario is Exist1 (normal model year) unless otherwise specified
load_ssm_summary <- function(year, scenario = "Exist1") {
  fpath <- here("data", "ssm", 
                paste("SSM", year, scenario, "summary.csv", 
                      sep = "_"))
  read_csv(fpath, 
           col_types = cols(GridCell = col_integer(), 
                            Month = col_integer(), 
                            Layer = col_integer(), 
                            DO_mgL_mean = col_double(), 
                            DO_mgL_sd = col_double(), 
                            DO_mgL_max = col_double(), 
                            DO_mgL_min = col_double(), 
                            Nitrate_mgL_mean = col_double(), 
                            Nitrate_mgL_sd = col_double(), 
                            Nitrate_mgL_max = col_double(), 
                            Nitrate_mgL_min = col_double(), 
                            Ammonia_mgL_mean = col_double(), 
                            Ammonia_mgL_sd = col_double(), 
                            Ammonia_mgL_max = col_double(), 
                            Ammonia_mgL_min = col_double(), 
                            Chla_mean = col_double(), 
                            Chla_sd = col_double(), 
                            Chla_max = col_double(), 
                            Chla_min = col_double())) %>% 
    mutate(Year = year, .before = GridCell) %>% 
    mutate(Scenario = scenario, .before = Year) %>% 
    relocate(GridCell, .before = Layer)
}

# Load in KC locator info: names, coordinates, SSM grid cells
load_locator_kc <- function() {
  read_csv(here("data", "whidbey_locators_kc.csv"), 
           col_types = cols(Number = col_skip(), 
                            Name = col_character(), 
                            Locator = col_character(), 
                            Lat = col_double(), 
                            Lon = col_double(), 
                            SiteType = col_skip(), 
                            Area = col_skip(), 
                            GridCell = col_integer()))
}

# Load in all Whidbey locator info: names, coordinates, agency, etc.
load_locator_all <- function(jitter = T) {
  read_csv(here("data", paste0("whidbey_stations_all", 
                               if_else(jitter, "_jitter.csv", ".csv"))), 
           col_types = cols(Name = col_character(), 
                            Agency = col_character(), 
                            Data_Type = col_character(), 
                            Frequency = col_character(), 
                            First_Year = col_integer(), 
                            Last_Year = col_integer(), 
                            Lat = col_double(), 
                            Lon = col_double(), 
                            DO = col_skip(), 
                            NO3 = col_skip(), 
                            PO4 = col_skip(), 
                            Chla = col_skip(), 
                            Si = col_skip(), 
                            Notes = col_character()))
}

# Load SSM summaries, combine into single tibble
# Assumes that all columns in SSM summaries are the same
combine_ssm_summary <- function(years, scenarios) {
  temp <- list()
  for (i in 1:length(years)) {
    if (length(scenarios) == 1) {
      temp[[i]] <- load_ssm_summary(years[i], scenarios)
    } else if (length(scenarios) == length(years)) {
      temp[[i]] <- load_ssm_summary(years[i], scenarios[i])
    } else 
      stop("scenarios vector is not length 1 or length(years)")
  }
  
  output <- temp[[1]]
  for (j in 2:length(years)) {
    output <- add_row(output, temp[[j]])
  }
  return(output)
}

# Re-download Whidbey Basin discrete/bottle data
download_whidbey_discrete <- function() {
  fpath <- here("data", "raw", "whidbey_discrete.csv")
  download_discrete(sites = c("PENNCOVECW", 
                              "PENNCOVEENT", 
                              "PENNCOVEWEST", 
                              "PSUSANBUOY", 
                              "SARATOGACH"), 
                    parms_in = c("NH3N", 
                                 "ChlorophyllA", 
                                 "LightTrans", 
                                 "NNN", 
                                 "OP", 
                                 "PheophytinA", 
                                 "SecchiTrans", 
                                 "Silica", 
                                 "TotalN", 
                                 "TSS"), 
                    fname = fpath)
}
