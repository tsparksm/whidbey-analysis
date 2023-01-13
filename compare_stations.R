#### Setup ####
source(here::here("src", "utility_functions.R"))

data_whidbey <- load_ctd_data() %>% 
  mutate(Month = month(Date), 
         Year = year(Date))
data_central <- load_composite(bin_size = 0.5, 
                               monthly = FALSE)

good_quals_ctd <- c(NA, "TA")

#### Combine data from Whidbey and Central Basins ####
data_combine <- full_join(data_whidbey, data_central)

#### Figure - compare 2 stations over a year, N and P ####
stations <- c("SARATOGACH", "NSEX01")
yoi <- 2022

ggplot(data = data_combine %>% 
         filter(Locator %in% stations, 
                Year == yoi, 
                ParmId %in% c(14, 15)), 
       aes(x = Date, 
           y = )