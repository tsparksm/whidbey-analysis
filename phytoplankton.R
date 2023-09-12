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
  "SARATOGACH", 
  "PENNCOVEENT", 
  "PSUSANBUOY", 
  "KSBP01", 
  "NSAJ02", 
  "LTED04" 
)

yoi <- 2022

data_to_plot <- phyto_data %>% 
  filter(locator %in% locators, 
         year(collect_date) == yoi) %>% 
  mutate(Basin = ifelse(locator %in% locators[1:3], 
                        "Whidbey", 
                        "Central"))

data_to_plot$locator <- factor(
  x = data_to_plot$locator, 
  levels = locators
)

ggplot(data_to_plot, 
       aes(x = collect_date, y = total_biovolume)) + 
  geom_point() + 
  facet_wrap(~ locator + Basin, 
             labeller = labeller(.multi_line = FALSE)) + 
  theme_bw() + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) + 
  labs(x = "", y = expression(Total~biovolume~(mm^3/L)))
ggsave(here("figs", "phyto", 
            paste0("Central_vs_Whidbey_biovolume_", yoi, ".png")), 
       dpi = 600, height = 4, width = 6)
