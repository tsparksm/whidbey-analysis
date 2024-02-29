#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)

yoi <- 2023

#### Create file for import in Socrata - surface ####
# Will use GUI for load and save file selection
start_time <- "11:30"  # 24 hr HH:MM
start_date <- "2024-01-04"  # YYYY-MM-DD
process_socrata_penncovesurf(start_date, start_time)

#### Combine SUNA and HCEP data - bottom ####
data_HCEP <- load_PC_bottom_HCEP()
data_SUNA <- load_PC_bottom_SUNA()

data_SUNA_processed <- data_SUNA %>% 
  group_by(SUNA_id, DateTime) %>% 
  summarize(NO3_umol = mean(NO3_umol, na.rm = T), 
            NO3_mgNL = mean(NO3_mgNL, na.rm = T), 
            NO3_n = n()) %>% 
  mutate(DateTime = with_tz(DateTime, tzone = "Etc/GMT+8"))

data_HCEP_1 <- data_HCEP %>% 
  filter(row_number() <= 3616)
tz(data_HCEP_1$Datetime) <- "Etc/GMT+7"
data_HCEP_1 <- data_HCEP_1 %>% 
  mutate(DateTime = with_tz(Datetime, tzone = "Etc/GMT+8"))
data_HCEP_2 <- data_HCEP %>% 
  filter(row_number() > 3616) %>% 
  mutate(DateTime = Datetime)
tz(data_HCEP_2$DateTime) <- "Etc/GMT+8"
data_HCEP_processed <- data_HCEP_1 %>% 
  add_row(data_HCEP_2) %>% 
  relocate(DateTime, .after = HCEP_id) %>% 
  select(-Datetime)

data_combined <- full_join(data_HCEP_processed, data_SUNA_processed) %>% 
  arrange(DateTime) %>% 
  filter(row_number() >= 4, 
         row_number() <= n() - 3)

fpath <- here("data", "socrata", paste0("penncovebottom_socrata_", 
                                        Sys.Date(), 
                                        ".csv"))

write_csv(data_combined, fpath)

#### Load Socrata data and combine ####
surf_data <- load_penncovesurf() %>% 
  rename_with(~str_c(., "_surface"), .cols = Temperature:NO23)
bottom_data <- load_penncovebottom() %>% 
  rename_with(~str_c(., "_bottom"), .cols = HCEP_id:NO3_n)
combo_data <- full_join(surf_data, bottom_data)

#### Figure: year of DO, top vs bottom ####
data_to_plot <- combo_data %>% 
  select(DateTime, Oxygen_mgL_bottom, Oxygen_surface) %>% 
  rename(Bottom = Oxygen_mgL_bottom, 
         Surface = Oxygen_surface) %>% 
  pivot_longer(cols = !DateTime, 
               names_to = "Location", 
               values_to = "DO")
ggplot(data = data_to_plot, 
       aes(x = DateTime, 
           y = DO, 
           color = Location)) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  geom_point() + 
  scale_y_continuous(limits = c(0, 15)) + 
  scale_color_brewer(palette = "Paired") + 
  labs(x = "", 
       y = "Oxygen (mg/L)", 
       title = "Penn Cove", 
       color = "")
ggsave(here("figs", "penncove", "surface_vs_bottom_do.png"), 
       height = 6, width = 10)
