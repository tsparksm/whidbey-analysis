#### SETUP ####
source(here::here("src", "utility_functions.R"))
library(rtide)

yoi <- 2024

#### Load Socrata data and combine ####
surf_data <- load_penncovesurf() %>% 
  rename_with(~str_c(., "_surface"), .cols = Temperature:NO23)
bottom_data <- load_penncovebottom() %>% 
  rename_with(~str_c(., "_bottom"), .cols = HCEP_id:NO3_n)
combo_data <- full_join(surf_data, bottom_data)

#### Load CTD and discrete data ####
discrete_data <- load_whidbey_discrete()
ctd_data <- load_CTD("PENNCOVEENT")

#### Figure: year of DO, top vs bottom ####
yoi <- 2024
data_to_plot <- combo_data %>% 
  select(DateTime, Oxygen_mgL_bottom, Oxygen_surface) %>% 
  rename(Bottom = Oxygen_mgL_bottom, 
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
  scale_y_continuous(limits = c(0, 15)) + 
  scale_color_brewer(palette = "Paired") + 
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
  select(DateTime, NO3_mgNL_bottom, NO23_surface) %>% 
  rename(Bottom = NO3_mgNL_bottom, 
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
  scale_y_continuous(limits = c(0, 1)) + 
  scale_color_brewer(palette = "Paired") + 
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
  select(DateTime, Chlorophyll_ugL_bottom, Chlorophyll_surface) %>% 
  rename(Bottom = Chlorophyll_ugL_bottom, 
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
  scale_y_continuous(limits = c(0, 15)) + 
  scale_color_brewer(palette = "Paired") + 
  labs(x = "", 
       y = expression(Chlorophyll~(mu*g/L)), 
       title = paste(yoi, "Penn Cove"),  
       color = "")
ggsave(here("figs", "penncove", 
            paste(yoi, "_surface_vs_bottom_chl.png")), 
       height = 6, width = 10)

