#### Setup ####
source(here::here("src", "utility_functions.R"))
library(RSocrata)

phyto_data <- get_phyto()

whidbey_stations <- c(
  "PENNCOVEENT", 
  "PENNCOVECW", 
  "PENNCOVEWEST", 
  "PSUSANBUOY", 
  "SARATOGACH"
)

#### Whidbey total biovolume over time ####
phyto_data %>% 
  filter(locator %in% whidbey_stations) %>% 
  ggplot(aes(x = collect_date, y = total_biovolume)) + 
  geom_point() + 
  facet_wrap(~locator)

#### Whidbey vs Central for 1 year ####
locators <- c(
  "KSBP01", 
  "LTED04", 
  "NSAJ02", 
  "SARATOGACH", 
  "PENNCOVEENT", 
  "PSUSANBUOY"
)

yoi <- 2022

phyto_data %>% 
  filter(locator %in% locators, 
         year(collect_date) == yoi) %>% 
  ggplot(aes(x = collect_date, y = total_biovolume)) + 
  geom_point() + 
  facet_wrap(~locator) + 
  theme_bw() + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) + 
  labs(x = "", y = expression(Total~biovolume~(mm^3/L)))
