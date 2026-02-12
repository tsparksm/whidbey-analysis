#### Setup ####
source(here::here("src", "utility_functions.R"))

data_climate <- load_climate() %>% 
  rename(Date = DATE) %>% 
  mutate(Year = year(Date))

data_strat <- load_strat()

#### Single year wind ####
yoi <- if (!exists("yoi")) {yoi <- as.numeric(readline("Year of interest: "))}

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
start_date <- if (!exists("start_date")) {start_date <- readline("Start date: ")}
end_date <- if (!exists("end_date")) {end_date <- readline("End date: ")}
poi <- as.Date(c(start_date, end_date))
station <- if (!exists("station")) {station <- readline("Station of interest: ")}
data_to_plot <- data_climate %>% 
  select(Date, AWND) %>% 
  full_join(filter(data_strat, Locator == station))

ggplot(data = data_to_plot, 
       aes(x = Date)) + 
  theme_bw() + 
  geom_line(aes(y = AWND)) + 
  geom_point(aes(y = Max_buoyfreq/2), ) + 
  scale_x_date(limits = poi) + 
  scale_y_continuous(limits = c(0, NA)) + 
  labs(x = "", 
       y = "Daily avg. wind speed (mph)", 
       title = paste(poi[1], "to", poi[2], station))
ggsave(here("figs", "strat", 
            paste(poi[1], poi[2], "strat_wind.png", sep = "_")), 
       width = 6, height = 4, dpi = 600)
