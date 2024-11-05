#### Setup ####
source(here::here("src", "utility_functions.R"))
library(waterData)

#### Load data ####
stillaguamish <- importDVs("12167000", 
                           sdate = "2022-01-01") %>% 
  mutate(River = "Stillaguamish")
snohomish <- importDVs("12150800", 
                       sdate = "2022-01-01") %>% 
  mutate(River = "Snohomish")
skagit <- importDVs("12200500", 
                    sdate = "2022-01-01") %>% 
  mutate(River = "Skagit")

long_data <- full_join(stillaguamish, snohomish) %>% 
  full_join(skagit) %>% 
  select(-staid, -qualcode) %>% 
  mutate(Year = year(dates))

wide_data <- pivot_wider(long_data, 
                         names_from = River, 
                         values_from = val)

#### Plot year of river flows - single figure ####
yoi <- 2023

ggplot(data = long_data %>% 
         filter(Year == yoi), 
       aes(x = dates, 
           y = val, 
           color = River)) + 
  theme_bw() + 
  geom_line() + 
  scale_x_date(expand = c(0, 0), 
               date_breaks = "1 month", 
               minor_breaks = "1 month", 
               date_labels = "%b") + 
  scale_color_brewer(palette = "Dark2") + 
  labs(x = "", 
       y = expression(Daily~mean~flow~(ft^3/s)), 
       title = paste(yoi, "River Flows"), 
       color = "")
ggsave(here("figs", 
            paste0("river_flow_combined_", yoi, ".png")), 
       dpi = 600, 
       height = 4, width = 8)

#### Plot year of river flows - multipanel ####
yoi <- 2024

ggplot(data = long_data %>% 
         filter(Year == yoi), 
       aes(x = dates, 
           y = val)) + 
  theme_bw() + 
  facet_wrap(~ River, ncol = 1, scales = "free") + 
  geom_line() + 
  scale_x_date(expand = c(0, 0), 
               date_breaks = "1 month", 
               minor_breaks = "1 month", 
               date_labels = "%b") + 
  labs(x = "", 
       y = expression(Daily~mean~flow~(ft^3/s)), 
       title = paste(yoi, "River Flows"))
ggsave(here("figs", 
            paste0("river_flow_multipanel_", yoi, ".png")), 
       dpi = 600, height = 6, width = 8)
