#### Setup ####
source(here::here("src", "utility_functions.R"))
library(patchwork)

# Identify years and scenarios to include
years <- c(2006, 2008, 2014)
scenarios <- "Exist1"  # 1 scenario, or length(years) scenarios

#### Load data and information ####
locator_info <- load_locator_kc() %>% 
  select(Locator, GridCell)
data_ctd <- load_composite(bin_size = 0.5, 
                           monthly = FALSE) %>% 
  mutate(Type = "CTD", 
         Month = month(Date)) %>% 
  left_join(locator_info)
data_discrete <- load_whidbey_discrete() %>% 
  mutate(Type = "Discrete") %>% 
  left_join(locator_info)
data_ssm <- combine_ssm_summary(years, scenarios) %>% 
  filter(GridCell %in% unique(locator_info$GridCell)) %>% 
  left_join(locator_info)
  
# Create "average" SSM output
data_ssm_avg <- data_ssm %>% 
  group_by(Locator, Layer, Month) %>% 
  summarize(Depth = mean(Depth_m_mean), 
            DO = mean(DO_mgL_mean), 
            DO_max = max(DO_mgL_max), 
            DO_min = min(DO_mgL_min), 
            NO23 = mean(Nitrate_mgL_mean), 
            NO23_max = max(Nitrate_mgL_max), 
            NO23_min = min(Nitrate_mgL_min), 
            NH3 = mean(Ammonia_mgL_mean), 
            NH3_max = max(Ammonia_mgL_max), 
            NH3_min = min(Ammonia_mgL_min), 
            Chlorophyll = mean(Chla_mean), 
            Chlorophyll_max = max(Chla_max), 
            Chlorophyll_min = min(Chla_min)) %>% 
  mutate(Type = "SSM")

# Combine SSM and CTD output
data_combined_ctd <- full_join(data_ctd, data_ssm_avg)

# Combine SSM and discrete output ####
# TO DO

#### Figure - CTD vs SSM DO; all stations, months ####
yoi <- 2024
for (station in unique(locator_info$Locator)) {
  for (month in 1:12) {
    ggplot(data = data_combined_ctd %>% 
             filter(Locator == station, 
                    Month == month) %>% 
             mutate(HiLite = case_when(
               Year == yoi ~ as.character(yoi), 
               is.na(Year) ~ "SSM", 
               TRUE ~ paste0("2021-", yoi-1)
             ))) + 
      theme_bw() + 
      geom_ribbon(aes(x = Depth, 
                      ymin = DO_min, 
                      ymax = DO_max, 
                      group = Type), 
                  fill = "light gray") + 
      geom_point(aes(x = Depth, 
                     y = DO, 
                     shape = Type), 
                 color = "dark gray") + 
      geom_line(aes(x = Depth, 
                    y = DO, 
                    color = HiLite, 
                    group = Date)) + 
      scale_shape_manual(values = c(NA, 16)) + 
      scale_color_manual(values = c("black", "red", "dark grey")) + 
      coord_flip() + 
      scale_x_reverse(expand = c(0, 0)) + 
      labs(x = "Depth (m)", 
           y = "DO (mg/L)", 
           color = "", 
           shape = "", 
           title = paste(station, "-", month.name[month]))
    ggsave(filename = here("figs", "ssm", "do", 
                           paste0(station, "_", month, "_DO.png")), 
           height = 5, width = 5)
  }
}

#### Figure - CTD vs SSM N; all stations, months ####
for (station in unique(locator_info$Locator)) {
  for (month in 1:12) {
    ggplot(data = data_combined_ctd %>% 
             filter(Locator == station, 
                    Month == month)) + 
      theme_bw() + 
      geom_ribbon(aes(x = Depth, 
                      ymin = NO23_min, 
                      ymax = NO23_max, 
                      group = Type), 
                  fill = "light gray") + 
      geom_line(aes(x = Depth, 
                    y = NO23, 
                    color = Type, 
                    group = Date)) + 
      geom_point(aes(x = Depth, 
                     y = NO23, 
                     shape = Type), 
                 color = "dark gray") + 
      scale_shape_manual(values = c(NA, 16)) + 
      scale_color_manual(values = c("black", "dark gray")) + 
      coord_flip() + 
      scale_x_reverse(expand = c(0, 0)) + 
      labs(x = "Depth (m)", 
           y = "Nitrate+nitrite N (mg/L)", 
           color = "", 
           shape = "", 
           title = paste(station, "-", month.name[month]))
    ggsave(filename = here("figs", "ssm", "no23", 
                           paste0(station, "_", month, "_NO23.png")), 
           height = 5, width = 5)
  }
}

#### Figure - CTD vs SSM DO and N; 1 station, 2 months ####
station <- "PENNCOVEENT"
months <- c(3, 9)

p1 <- ggplot(data = data_combined_ctd %>% 
         filter(Locator == station, 
                Month %in% months) %>% 
         mutate(Month = month.name[Month])) + 
  theme_bw() + 
  geom_ribbon(aes(x = Depth, 
                  ymin = DO_min, 
                  ymax = DO_max, 
                  group = Type), 
              fill = "light gray") + 
  facet_wrap(~ Month) + 
  geom_line(aes(x = Depth, 
                y = DO, 
                color = Type, 
                group = Date)) + 
  geom_point(aes(x = Depth, 
                 y = DO, 
                 shape = Type), 
             color = "dark gray") + 
  scale_shape_manual(values = c(NA, 16)) + 
  scale_color_manual(values = c("black", "dark gray")) + 
  coord_flip() + 
  scale_x_reverse(expand = c(0, 0)) + 
  labs(x = "Depth (m)", 
       y = "DO (mg/L)", 
       color = "", 
       shape = "")

p2 <- ggplot(data = data_combined_ctd %>% 
               filter(Locator == station, 
                      Month %in% months) %>% 
               mutate(Month = month.name[Month])) + 
  theme_bw() + 
  geom_ribbon(aes(x = Depth, 
                  ymin = NO23_min, 
                  ymax = NO23_max, 
                  group = Type), 
              fill = "light gray") + 
  facet_wrap(~ Month) + 
  geom_line(aes(x = Depth, 
                y = NO23, 
                color = Type, 
                group = Date)) + 
  geom_point(aes(x = Depth, 
                 y = NO23, 
                 shape = Type), 
             color = "dark gray") + 
  scale_shape_manual(values = c(NA, 16)) + 
  scale_color_manual(values = c("black", "dark gray")) + 
  coord_flip() + 
  scale_x_reverse(expand = c(0, 0)) + 
  labs(x = "Depth (m)", 
       y = "Nitrate + nitrite (mg N/L)", 
       color = "", 
       shape = "")

p1 / p2 + plot_layout(guides = "collect")
ggsave(here("figs", "ssm", 
            paste0(station, "_", 
                   paste(months, collapse = "_"), 
                   ".png")), 
       dpi = 600, height = 6, width = 6)

#### Figure - CTD vs SSM DO and N; 1 station, 2 months, 1 year ####
station <- "PENNCOVEENT"
months <- c(3, 9)
yoi <- 2024

p1 <- ggplot(data = data_combined_ctd %>% 
               filter(Locator == station, 
                      Month %in% months, 
                      Year == yoi | Type == "SSM") %>% 
               mutate(Month = month.name[Month])) + 
  theme_bw() + 
  geom_ribbon(aes(x = Depth, 
                  ymin = DO_min, 
                  ymax = DO_max, 
                  group = Type), 
              fill = "light gray") + 
  facet_wrap(~ Month) + 
  geom_line(aes(x = Depth, 
                y = DO, 
                color = Type, 
                group = Date)) + 
  geom_point(aes(x = Depth, 
                 y = DO, 
                 shape = Type), 
             color = "dark gray") + 
  scale_shape_manual(values = c(NA, 16)) + 
  scale_color_manual(values = c("black", "dark gray")) + 
  coord_flip() + 
  scale_x_reverse(expand = c(0, 0)) + 
  labs(x = "Depth (m)", 
       y = "DO (mg/L)", 
       color = "", 
       shape = "")

p2 <- ggplot(data = data_combined_ctd %>% 
               filter(Locator == station, 
                      Month %in% months, 
                      Year == yoi | Type == "SSM") %>% 
               mutate(Month = month.name[Month])) + 
  theme_bw() + 
  geom_ribbon(aes(x = Depth, 
                  ymin = NO23_min, 
                  ymax = NO23_max, 
                  group = Type), 
              fill = "light gray") + 
  facet_wrap(~ Month) + 
  geom_line(aes(x = Depth, 
                y = NO23, 
                color = Type, 
                group = Date)) + 
  geom_point(aes(x = Depth, 
                 y = NO23, 
                 shape = Type), 
             color = "dark gray") + 
  scale_shape_manual(values = c(NA, 16)) + 
  scale_color_manual(values = c("black", "dark gray")) + 
  coord_flip() + 
  scale_x_reverse(expand = c(0, 0)) + 
  labs(x = "Depth (m)", 
       y = "Nitrate + nitrite (mg N/L)", 
       color = "", 
       shape = "")

p1 / p2 + plot_layout(guides = "collect")
ggsave(here("figs", "ssm", 
            paste0(station, "_", 
                   paste(months, collapse = "_"), "_", 
                   yoi, 
                   ".png")), 
       dpi = 600, height = 6, width = 6)

#### Figure - CTD vs SSM DO; 1 station, 2 months ####
station <- "PENNCOVEENT"
months <- c(3, 9)

ggplot(data = data_combined_ctd %>% 
               filter(Locator == station, 
                      Month %in% months) %>% 
               mutate(Month = month.name[Month])) + 
  theme_bw() + 
  geom_ribbon(aes(x = Depth, 
                  ymin = DO_min, 
                  ymax = DO_max, 
                  group = Type), 
              fill = "light gray") + 
  facet_wrap(~ Month) + 
  geom_line(aes(x = Depth, 
                y = DO, 
                color = Type, 
                group = Date)) + 
  geom_point(aes(x = Depth, 
                 y = DO, 
                 shape = Type), 
             color = "dark gray") + 
  scale_shape_manual(values = c(NA, 16)) + 
  scale_color_manual(values = c("black", "dark gray")) + 
  coord_flip() + 
  scale_x_reverse(expand = c(0, 0)) + 
  labs(x = "Depth (m)", 
       y = "DO (mg/L)", 
       color = "", 
       shape = "")

ggsave(here("figs", "ssm", 
            paste0(station, "_DO_", 
                   paste(months, collapse = "_"), 
                   ".png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - CTD vs SSM DO; 1 station, 2 months, 1 year ####
station <- "PENNCOVEENT"
months <- c(3, 9)
yoi <- 2024

ggplot(data = data_combined_ctd %>% 
               filter(Locator == station, 
                      Month %in% months, 
                      Year == yoi | Type == "SSM") %>% 
               mutate(Month = month.name[Month])) + 
  theme_bw() + 
  geom_ribbon(aes(x = Depth, 
                  ymin = DO_min, 
                  ymax = DO_max, 
                  group = Type), 
              fill = "light gray") + 
  facet_wrap(~ Month) + 
  geom_line(aes(x = Depth, 
                y = DO, 
                color = Type, 
                group = Date)) + 
  geom_point(aes(x = Depth, 
                 y = DO, 
                 shape = Type), 
             color = "dark gray") + 
  scale_shape_manual(values = c(NA, 16)) + 
  scale_color_manual(values = c("black", "dark gray")) + 
  coord_flip() + 
  scale_x_reverse(expand = c(0, 0)) + 
  labs(x = "Depth (m)", 
       y = "DO (mg/L)", 
       color = "", 
       shape = "")

ggsave(here("figs", "ssm", 
            paste0(station, "_DO_", 
                   paste(months, collapse = "_"), "_", 
                   yoi, 
                   ".png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - Penn Cove minimum DO by year #### 
# TO DO
data_to_plot <- data_ssm %>% 
  filter(Locator == "PENNCOVEENT") %>% 
  group_by()
  summarize(MinDO = min())

ggplot(data_to_plot, 
       )