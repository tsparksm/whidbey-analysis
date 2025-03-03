#### Setup ####
source(here::here("src", "utility_functions.R"))

data_climate <- load_climate() %>% 
  rename(Date = DATE) %>% 
  mutate(Year = year(Date))

data_strat <- load_strat()

#### Single year wind ####
yoi <- 2024

data_to_plot <- data_climate %>% 
  filter(Year == yoi)

ggplot(data = data_to_plot, 
       aes(x = Date, 
           y = AWND)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b", 
               expand = c(0, 0)) + 
  labs(x = "", y = "Daily avg. wind speed (mph)", title = yoi)
ggsave(here("figs", paste0("Everett_winds_", yoi, ".png")), 
       width = 6, height = 4, dpi = 600)

#### Short period strat & wind ####
poi <- as.Date(c("2022-09-01", "2022-10-31"))
station <- "SARATOGACH"
data_to_plot <- data_climate %>% 
  select(Date, AWND) %>% 
  full_join(filter(data_strat, Locator == station))

ggplot(data = data_to_plot, 
       aes(x = Date)) + 
  theme_bw() + 
  geom_line(aes(y = AWND)) + 
  geom_point(aes(y = Max_buoyfreq)) + 
  scale_x_date(limits = poi) + 
  scale_y_continuous(limits = c(0, 50)) + 
  labs(x = "", 
       y = "Daily avg. wind speed (mph)", 
       title = paste(poi[1], "to", poi[2], station))
ggsave(here("figs", "strat", 
            paste(poi[1], poi[2], "strat_wind.png", sep = "_")), 
       width = 6, height = 4, dpi = 600)
