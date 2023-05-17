library(kcmarine)
library(tidyverse)
library(here)
library(lubridate)

# Load CTD data - single or multiple sites
# Outputs a single tibble containing one or more sites
# sites = vector of 1+ locator codes, e.g. c("JSUR01", "LTED04")
# type = "qcd" (default) or "raw"
load_CTD <- function(sites, type = "qcd") {
  folder <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/"
  for (locator in sites) {
    subfolder <- paste0(folder, locator)
    fpath <- list.files(path = subfolder, 
                        pattern = paste0(type, ".csv"), 
                        full.names = T)
    temp <- import_CTD(fpath)
    if (locator == sites[1]) {
      output <- temp
    } else {
      output <- add_row(output, temp)
    }
  }
  return(output)
}

# Load in Whidbey Basin discrete/bottle data
# Data must be downloaded using kcmarine download_discrete()
# Adds depth bins that are relevant for Whidbey Basin sites
load_whidbey_discrete <- function() {
  fpath <- here("data", "raw", "whidbey_discrete.csv")
  output <- import_discrete(fpath) %>% 
    mutate(Detect = !is.na(Value), 
           Value = ifelse(Detect, Value, Mdl), 
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

# Load in Port Susan EXO data (ST)
load_buoy_data_ST <- function() {
  fpath <- here("data", "raw", "prelim_PT_SUSAN_BUOY_DATA_MASTER.csv")
  read_csv(fpath, 
           col_types = cols(Timestamp = col_datetime(format = "%m/%d/%Y %H:%M"), 
                            Date = col_date(format = "%m/%d/%Y"), 
                            Time = col_time(), 
                            TimeSec = col_integer(), 
                            SiteName = col_character(), 
                            Chlorophyll_RFU = col_number(), 
                            Chlorophyll_ugL = col_number(), 
                            Cond_uScm = col_number(), 
                            Depth_m = col_number(), 
                            nLF_Cond_uScm = col_number(), 
                            ODO_percent_sat = col_number(), 
                            ODO_percent_local = col_number(), 
                            ODO_mgL = col_number(), 
                            Pressure_psi_a = col_number(), 
                            Salinity_psu = col_number(), 
                            SpCond = col_number(), 
                            TAL_PE_RFU = col_number(), 
                            TAL_PE_ugL = col_number(), 
                            TDS_mgL = col_number(), 
                            Turbidity_FNU = col_number(), 
                            TSS_mgL = col_number(), 
                            Wiper_Position_V = col_number(), 
                            pH = col_number(), 
                            pH_mV = col_number(), 
                            Temperature_degC = col_number(), 
                            Vertical_Pos_m = col_number(), 
                            Battery_V = col_number(), 
                            Cable_Pwr_V = col_number(), 
                            Probe_SN = col_character(), 
                            Comments = col_character())) %>% 
    rename(Temperature = Temperature_degC, 
           Oxygen = ODO_mgL, 
           Chlorophyll = Chlorophyll_ugL, 
           OxygenSat = ODO_percent_sat, 
           Salinity = Salinity_psu) %>% 
    mutate(DateTime = as.POSIXct(paste(Date, Time), 
                                 format = "%Y-%m-%d %H:%M:%S")) %>% 
    select(DateTime, 
           Temperature, 
           Oxygen, 
           Chlorophyll, 
           OxygenSat, 
           Salinity)
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
  read_csv(here("data", "whidbey_stations_kc.csv"), 
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

# Calculate depth bin based on total profile depth and bin width
depth_bin <- function(depth, width) {
  end <- ceiling(max(depth) / width) * width
  break_points <- seq(from = 0, to = end, by = width)
  bins <- cut(depth, breaks = break_points)
  return(bins)
}

# Using output from depth_bin(), calculate midpoint of bin
get_bin_depth <- function(bin) {
  depths <- parse_number(unlist(strsplit(as.character(bin), ",")))
  depth <- mean(depths)
  return(depth)
}

# Load CTD composite - Central Basin or Whidbey monitoring
# For Whidbey, set location = "Whidbey" (only 0.5 and 1 m bins right now)
# For Central Basin, set location = "" or "Central"
load_composite <- function(bin_size, 
                           location = "Whidbey", 
                           monthly = TRUE) {
  if (location == "Whidbey") location <- "Whidbey_"
  if (location == "Central") location <- ""
  folder <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/Data composites/"
  import_CTD_composite(paste0(folder, "data_", 
                              location, 
                              "CTD_", 
                              bin_size, "m_bins_", 
                              if_else(monthly, "monthly", "all"), 
                              ".csv"))
}

# Load stratification - Central Basin or Whidbey
# For Whidbey, set location = "Whidbey"
# For Central Basin, set location = "" or "Central"
load_strat <- function(location = "Whidbey") {
  if (location == "Whidbey") location <- "Whidbey_"
  if (location == "Central") location <- ""
  folder <- "//kc.kingcounty.lcl/dnrp/WLRD/STS/Share/Marine Group/CTD_data_repository/Data composites/"
  fname <- paste0(folder, "data_", location, "strat_all.csv")
  read_csv(fname, 
           col_types = cols(Locator = col_character(), 
                            Date = col_date(), 
                            Stratified = col_logical(), 
                            Max_dpdz = col_double(), 
                            Depth_pycno = col_double(), 
                            Max_buoyfreq = col_double(), 
                            Pycno_dpdS = col_double(), 
                            Pycno_dpdT = col_double(), 
                            Mean_density = col_double(), 
                            PE_anomaly = col_double(), 
                            DeltaT = col_double(), 
                            DeltaS = col_double(), 
                            DeltaP = col_double(), 
                            DeltaT_50 = col_double(), 
                            DeltaS_50 = col_double(), 
                            DeltaP_50 = col_double(), 
                            Max_depth = col_double(), 
                            Flag = col_character(), 
                            BinPycno = col_character(), 
                            BinStrength = col_character(), 
                            Month = col_integer(), 
                            Year = col_integer()))
}

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

craftbrewer_pal <- function (type = "seq", palette = 1, direction = 1) 
{
  pal <- scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

scale_fill_craftfermenter <- function(..., type = "seq", 
                                      palette = 1, 
                                      direction = -1, 
                                      na.value = "grey50", 
                                      guide = "coloursteps", 
                                      aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

# Load climate data from NOAA
# Daily summaries downloaded here: https://www.ncei.noaa.gov/cdo-web/search
# Click on Everett for best Whidbey options
load_climate <- function() {
  fname <- here("data", "raw", "everett_climate_data.csv")
  read_csv(fname, 
           col_types = cols(STATION = col_character(), 
                            NAME = col_character(), 
                            LATITUDE = col_double(), 
                            LONGITUDE = col_double(), 
                            ELEVATION = col_double(), 
                            DATE = col_date(), 
                            AWND = col_double(), 
                            AWND_ATTRIBUTES = col_character(), 
                            DAPR = col_integer(), 
                            DAPR_ATTRIBUTES = col_character(), 
                            MDPR = col_double(), 
                            MDPR_ATTRIBUTES = col_character(), 
                            PGTM = col_integer(), 
                            PGTM_ATTRIBUTES = col_character(), 
                            PRCP = col_double(), 
                            PRCP_ATTRIBUTES = col_character(), 
                            SNOW = col_double(), 
                            SNOW_ATTRIBUTES = col_character(), 
                            SNWD = col_double(), 
                            SNWD_ATTRIBUTES = col_character(), 
                            TAVG = col_double(), 
                            TAVG_ATTRIBUTES = col_character(), 
                            TMAX = col_integer(), 
                            TMAX_ATTRIBUTES = col_character(), 
                            TMIN = col_integer(), 
                            TMIN_ATTRIBUTES = col_character(), 
                            TOBS = col_integer(), 
                            TOBS_ATTRIBUTES = col_character(), 
                            WDF2 = col_integer(), 
                            WDF2_ATTRIBUTES = col_character(), 
                            WDF5 = col_integer(), 
                            WDF5_ATTRIBUTES = col_character(), 
                            WESD = col_double(), 
                            WESD_ATTRIBUTES = col_character(), 
                            WESF = col_double(), 
                            WESF_ATTRIBUTES = col_character(), 
                            WDF2 = col_double(), 
                            WDF2_ATTRIBUTES = col_character(), 
                            WDF5 = col_double(), 
                            WDF5_ATTRIBUTES = col_character(), 
                            WT01 = col_skip(), 
                            WT01_ATTRIBUTES = col_skip(), 
                            WT02 = col_skip(), 
                            WT02_ATTRIBUTES = col_skip(), 
                            WT03 = col_skip(), 
                            WT03_ATTRIBUTES = col_skip(), 
                            WT08 = col_skip(), 
                            WT08_ATTRIBUTES = col_skip()))
}
