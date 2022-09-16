library(kcmarine)
library(tidyverse)
library(here)

# Load in Whidbey Basin CTD data
# Data must be downloaded from internal CTD website
# Ensure ALL parameters are included
load_CTD_data <- function() {
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
load_SSM_summary <- function(year, scenario = "Exist1") {
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
    relocate(GridCell, .before = Layer)
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
