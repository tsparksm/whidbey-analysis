#### Setup ####
library(pspline)
library(zoo)
library(gsw)
source(here::here("src", "utility_functions.R"))

data_ctd <- load_ctd_data() %>% 
  mutate(Month = factor(month.name[month(Date)], 
                        levels = month.name), 
         Bin = depth_bin(Depth, 0.5), 
         BinDepth = sapply(Bin, get_bin_depth))
site_data <- load_locator_kc()

good_quals_ctd <- c(NA, "TA")

sites <- unique(data_ctd$Locator)

#### Make calculations ####
for (j in 1:length(sites)) {
  # Get locator/site name and cutoff depth
  site <- sites[j]
  
  # Filter data to specific locator
  temp_data <- data_ctd %>% filter(Locator == site)
  
  # Location data - for GSW calculations
  lat <- site_data %>% filter(Locator == site) %>% pull(Lat)
  long <- site_data %>% filter(Locator == site) %>% pull(Lon)
  
  # Get list of dates with profiles at this site
  dates <- unique(temp_data$Date)
  
  # Set up data structure for this site
  strat_data <- tibble(Locator = character(), 
                       Date = character(), 
                       Stratified = logical(), 
                       Max_dpdz = numeric(), 
                       Depth_pycno = numeric(), 
                       Max_buoyfreq = numeric(), 
                       Pycno_dpdS = numeric(), 
                       Pycno_dpdT = numeric(), 
                       Mean_density = numeric(), 
                       PE_anomaly = numeric(), 
                       DeltaT = numeric(), 
                       DeltaS = numeric(), 
                       DeltaP = numeric(), 
                       DeltaT_50 = numeric(), 
                       DeltaS_50 = numeric(), 
                       DeltaP_50 = numeric(), 
                       Max_depth = numeric(), 
                       Flag = character())
  
  for (i in 1:length(dates)) {
    # Filter to a single cast date
    d <- dates[i]
    fdata <- temp_data %>% filter(Date == d)
    
    # Determine what the maximum depth is, with bottom 2 m removed
    maxdepth <- max(fdata$Depth) - 2
    
    if (maxdepth < 20) next
    
    # Filter out top and bottom 2 m data
    # Arrange so that shallow data is at the top
    fdata <- fdata %>% 
      filter(Depth > 2, Depth <= maxdepth) %>%
      arrange(Depth)
    
    # Calculate top-to-bottom differences
    DT <- last(fdata$Temperature) - first(fdata$Temperature)
    DS <- last(fdata$Salinity) - first(fdata$Salinity)
    DP <- last(fdata$SigmaTheta) - first(fdata$SigmaTheta)
    
    if (any(is.na(c(DT, DS, DP)))) {flag = "some bad T/S/p"}
    
    # Filter down data to top 50 m
    fdata_top50 <- fdata %>% 
      filter(Depth <= 50)
    n <- nrow(fdata_top50)  # total number of depths from 2-50 m
    
    # Calculate buoyancy frequency, top-to-50 m differences, and potential energy anomaly
    if (maxdepth >= 50) {
      DT_50 <- last(fdata_top50$Temperature) - first(fdata_top50$Temperature)
      DS_50 <- last(fdata_top50$Salinity) - first(fdata_top50$Salinity)
      DP_50 <- last(fdata_top50$SigmaTheta) - first(fdata_top50$SigmaTheta)
      
      # Calculate mean density and Simpson potential energy anomaly
      mean_density <- mean(fdata_top50$SigmaTheta, na.rm = T)
      pe_anomaly <- sum((fdata_top50$SigmaTheta - mean_density)*9.8*fdata_top50$Depth*0.5, na.rm = T) / 40
      
    } else {
      DT_50 <- NA
      DS_50 <- NA
      DP_50 <- NA
      mean_density <- NA
      pe_anomaly <- NA
    }
    
    if (any(is.na(c(DT_50, DS_50)))) {flag = "some bad T/S/p"}
    
    # Filter data down to non-NA T/S/p
    binned_data <- fdata_top50 %>% 
      filter(!(is.na(Density)), 
             !(is.na(Temperature)), 
             !(is.na(Salinity)))
    # Skip the calculations if there are no rows
    if (nrow(binned_data) == 0) {
      mean_density = NA
      pe_anomaly = NA
      stratified = NA 
      max_dpdz = NA
      depth_pycno = NA 
      max_buoyfreq = NA 
      pycno_dpds = NA
      pycno_dpdt = NA 
      flag = "bad T/S/p"
    } else {
      # Calculate buoyancy frequency
      binned_data <- binned_data %>% 
        group_by(BinDepth) %>% 
        summarize(Density = mean(Density, na.rm = T), 
                  Temperature = mean(Temperature, na.rm = T), 
                  Salinity = mean(Salinity, na.rm = T)) %>% 
        mutate(dpdz = predict(sm.spline(BinDepth, Density), BinDepth, 1), 
               dpdzMA = rollapply(dpdz, 5, mean, na.rm = T, fill = NA, partial = T), 
               buoyfreq = (9.8 / 1025 * dpdzMA)^0.5 * 3600 / (2*pi), 
               Pressure = gsw_p_from_z(-BinDepth, lat), 
               AbsSal = gsw_SA_from_SP(Salinity, Pressure, long, lat), 
               ConsTemp = gsw_CT_from_t(AbsSal, Temperature, Pressure))
      
      dp <- gsw_rho_first_derivatives(
        binned_data$AbsSal, 
        binned_data$ConsTemp, 
        binned_data$Pressure
      )
      
      binned_data <- binned_data %>% 
        mutate(dpdS = dp$drho_dSA, 
               dpdT = dp$drho_dCT)
      
      # Flag if some rows have been removed due to NA T/S/p
      if (nrow(binned_data) < nrow(fdata_top50)) {
        flag = "some bad T/S/p"
      } else {
        flag = NA
      }
      
      # Calculate some stuff
      # But not if all dpdz <= 0
      if (all(binned_data$dpdz <= 0)) {
        stratified = NA
        max_dpdz = NA
        depth_pycno = NA
        max_buoyfreq = NA
        pycno_dpds = NA
        pycno_dpdt = NA
        flag = "-dpdz"
      } else {
        stratified = max(binned_data$dpdzMA, na.rm = T) >= 0.1
        max_dpdz = max(binned_data$dpdzMA, na.rm = T)
        depth_pycno = binned_data$BinDepth[which.max(binned_data$dpdzMA)]
        max_buoyfreq = max(binned_data$buoyfreq, na.rm = T)
        pycno_dpds = binned_data$dpdS[which.max(binned_data$dpdzMA)]
        pycno_dpdt = binned_data$dpdT[which.max(binned_data$dpdzMA)]
      }
    }
    
    # Save values in strat_data
    strat_data <- strat_data %>% 
      add_row(Locator = site, 
              Date = as.character(d), 
              Stratified = stratified, 
              Max_dpdz = max_dpdz, 
              Depth_pycno = depth_pycno, 
              Max_buoyfreq = max_buoyfreq, 
              Pycno_dpdS = pycno_dpds, 
              Pycno_dpdT = pycno_dpdt, 
              Mean_density = mean_density, 
              PE_anomaly = pe_anomaly, 
              DeltaT = DT, 
              DeltaS = DS, 
              DeltaP = DP, 
              DeltaT_50 = DT_50, 
              DeltaS_50 = DS_50, 
              DeltaP_50 = DP_50, 
              Max_depth = maxdepth, 
              Flag = flag)
  }
  
  analysis_data <- strat_data %>% 
    mutate(BinPycno = case_when(
      !Stratified ~ "unstratified", 
      Depth_pycno >= 2 & Depth_pycno < 4 ~ "2-4 m", 
      Depth_pycno >= 4 & Depth_pycno < 6 ~ "4-6 m", 
      Depth_pycno >= 6 & Depth_pycno < 8 ~ "6-8 m", 
      Depth_pycno >= 8 & Depth_pycno < 10 ~ "8-10 m", 
      Depth_pycno >= 10 & Depth_pycno < 20 ~ "10-20 m", 
      Depth_pycno >= 20 & Depth_pycno < 30 ~ "20-30 m", 
      Depth_pycno >= 30 ~ ">30 m"
    )) %>% 
    mutate(BinStrength = case_when(
      Max_buoyfreq < 20 ~ "0-20", 
      Max_buoyfreq >= 20 & Max_buoyfreq < 40 ~ "20-40", 
      Max_buoyfreq >= 40 & Max_buoyfreq < 60 ~ "40-60", 
      Max_buoyfreq >= 60 & Max_buoyfreq < 80 ~ "60-80", 
      Max_buoyfreq >= 80 & Max_buoyfreq < 100 ~ "80-100", 
      Max_buoyfreq >= 100 ~ ">100"
    )) %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"), 
           Month = month(Date), Year = year(Date))
  analysis_data$BinPycno <- factor(analysis_data$BinPycno, 
                                   levels = c("unstratified", 
                                              "2-4 m", 
                                              "4-6 m", 
                                              "6-8 m", 
                                              "8-10 m", 
                                              "10-20 m", 
                                              "20-30 m", 
                                              ">30 m"))
  analysis_data$BinStrength <- factor(analysis_data$BinStrength, 
                                      levels = c("0-20", 
                                                 "20-40", 
                                                 "40-60", 
                                                 "60-80", 
                                                 "80-100", 
                                                 ">100"))
  
  if (site == sites[1]) {
    full_data <- analysis_data
  } else {
    full_data <- full_join(full_data, analysis_data)
  }
}

write_csv(full_data, 
          here("data", "stratification.csv"))
