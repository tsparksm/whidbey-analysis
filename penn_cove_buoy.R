#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)

yoi <- 2023

#### Create file for import in Socrata ####
# Will use GUI for load and save file selection
start_time <- "12:15"  # 24 hr HH:MM
start_date <- "2023-12-05"  # YYYY-MM-DD
process_socrata_penncovesurf(start_date, start_time)
