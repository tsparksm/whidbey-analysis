#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)
library(plotly)

#### Load data ####
# Remember to update file via Socrata download first!
raw_data <- load_coupeville()

# Remember to update discrete data first!
# download_whidbey_discrete()
bottle_data <- load_whidbey_discrete() %>% 
  filter(Locator == "PENNCOVECW")

ctd_data <- load_CTD("PENNCOVECW")

#### Load/generate tide data ####
tide_data <- tide_height(stations = "Seattle", 
                         minutes = 15, 
                         from = min(raw_data$Date) - 1, 
                         to = max(raw_data$Date) + 1, 
                         tz = "Etc/GMT-8")
comb_data <- left_join(raw_data, tide_data %>% select(-Station))

#### FIGURE - pressure ####
start_date <- as.Date("2023-12-01")
end_date <- as.Date("2024-01-01")

data_to_plot <- comb_data %>% 
  filter(between(Date, start_date, end_date))

p <- ggplot(data_to_plot, 
       aes(x = DateTime)) + 
  geom_line(aes(y = TideHeight), 
            color = "gray") + 
  geom_point(aes(y = Pressure), 
             size = 0.2) + 
  theme_bw()
ggplotly(p)

#### FIGURE - temperature ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- comb_data %>% 
  filter(between(Date, start_date, end_date))

p <- ggplot(data_to_plot, 
            aes(x = DateTime, 
                y = Temperature)) + 
  geom_point(aes(), 
             size = 0.2) + 
  theme_bw()
ggplotly(p)

#### FIGURE - salinity ####
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2025-01-01")

data_to_plot <- comb_data %>% 
  filter(between(Date, start_date, end_date))

p <- ggplot(data_to_plot, 
            aes(x = DateTime, 
                y = Salinity)) + 
  geom_point(aes(), 
             size = 0.2) + 
  theme_bw()
ggplotly(p)


