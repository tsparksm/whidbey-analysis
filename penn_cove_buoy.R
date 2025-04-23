#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)

yoi <- 2024

#### Load QC'd data and combine ####
surf_data <- load_qc_penncovesurf() %>% 
  rename_with(~str_c(., "_surface"), .cols = Temperature:OxygenSat_final)
bottom_data <- load_qc_penncovebottom() %>% 
  rename_with(~str_c(., "_bottom"), .cols = HCEP_id:OxygenSat_final)
combo_data <- full_join(surf_data, bottom_data)

#### Load CTD and discrete data ####
discrete_data <- load_whidbey_discrete()
ctd_data <- load_CTD("PENNCOVEENT")

#### Figure: year of DO, top vs bottom ####
yoi <- 2024
data_to_plot <- combo_data %>% 
  mutate(Oxygen_bottom = ifelse(Oxygen_final_bottom == 1, Oxygen_bottom, NA), 
         Oxygen_surface = ifelse(Oxygen_final_surface == 1, Oxygen_surface, NA)) %>% 
  select(DateTime, Oxygen_bottom, Oxygen_surface) %>% 
  rename(Bottom = Oxygen_bottom, 
         Surface = Oxygen_surface) %>% 
  pivot_longer(cols = !DateTime, 
               names_to = "Location", 
               values_to = "DO") %>% 
  filter(year(DateTime) == yoi)
ggplot(data = data_to_plot, 
       aes(x = DateTime, 
           y = DO, 
           color = Location)) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  geom_point() + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  labs(x = "", 
       y = "Oxygen (mg/L)", 
       title = paste(yoi, "Penn Cove"),  
       color = "")
ggsave(here("figs", "penncove", 
            paste(yoi, "_surface_vs_bottom_do.png")), 
       height = 6, width = 10)

#### Figure: year of NO23, top vs bottom ####
yoi <- 2024
data_to_plot <- combo_data %>% 
  mutate(NO23_bottom = ifelse(NO23_final_bottom == 1, NO23_bottom, NA), 
         NO23_surface = ifelse(NO23_final_surface == 1, NO23_surface, NA)) %>% 
  select(DateTime, NO23_bottom, NO23_surface) %>% 
  rename(Bottom = NO23_bottom, 
         Surface = NO23_surface) %>% 
  pivot_longer(cols = !DateTime, 
               names_to = "Location", 
               values_to = "NO23") %>% 
  filter(year(DateTime) == yoi, 
         NO23 >= 0)
ggplot(data = data_to_plot, 
       aes(x = DateTime, 
           y = NO23, 
           color = Location)) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  geom_point() + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  labs(x = "", 
       y = "Nitrate+nitrite (mg N/L)", 
       title = paste(yoi, "Penn Cove"),  
       color = "")
ggsave(here("figs", "penncove", 
            paste(yoi, "_surface_vs_bottom_no23.png")), 
       height = 6, width = 10)

#### Figure: year of chl, top vs bottom ####
yoi <- 2024
data_to_plot <- combo_data %>% 
  mutate(Chlorophyll_bottom = ifelse(Chlorophyll_final_bottom == 1, 
                                     Oxygen_bottom, NA), 
         Chlorophyll_surface = ifelse(Chlorophyll_final_surface == 1, 
                                 Chlorophyll_surface, NA)) %>% 
  select(DateTime, Chlorophyll_bottom, Chlorophyll_surface) %>% 
  rename(Bottom = Chlorophyll_bottom, 
         Surface = Chlorophyll_surface) %>% 
  pivot_longer(cols = !DateTime, 
               names_to = "Location", 
               values_to = "Chlorophyll") %>% 
  filter(year(DateTime) == yoi, 
         Chlorophyll > 0)
ggplot(data = data_to_plot, 
       aes(x = DateTime, 
           y = Chlorophyll, 
           color = Location)) + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  geom_point() + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  scale_color_brewer(palette = "Paired") + 
  labs(x = "", 
       y = expression(Chlorophyll~(mu*g/L)), 
       title = paste(yoi, "Penn Cove"),  
       color = "")
ggsave(here("figs", "penncove", 
            paste(yoi, "_surface_vs_bottom_chl.png")), 
       height = 6, width = 10)

#### Figure: short period DO ####
