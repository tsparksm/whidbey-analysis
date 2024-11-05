#### Setup ####
source(here::here("src", "utility_functions.R"))
library(pracma)
strat_data <- load_strat()
ctd_data <- load_composite(0.5, monthly = FALSE)

#### Pycnocline depth vs chl max depth ####
# Calculate chl max depth
yoi <- 2024

chlmax <- ctd_data %>% 
  group_by(Locator, Date) %>% 
  summarize(Depth_chlmax = Depth[which.max(Chlorophyll)])

data_to_plot <- strat_data %>% 
  filter(!(Locator %in% c("PENNCOVECW", 
                          "PENNCOVEPNN001", 
                          "PENNCOVEWEST", 
                          "PSUSANBUOY")), 
         Year == yoi) %>% 
  select(Locator, Date, Depth_pycno) %>% 
  left_join(chlmax)

ggplot(data = data_to_plot, 
       aes(x = Depth_pycno, 
           y = Depth_chlmax)) + 
  theme_bw() + 
  geom_point() + 
  facet_wrap(~ Locator, 
             scales = "free", 
             nrow = 2) + 
  labs(x = "Pycnocline depth (m)", 
       y = "Chl. max depth (m)", 
       title = yoi)
ggsave(here("figs", "strat", 
            paste0("pyc_depth_vs_chl_max_depth_", yoi, ".png")), 
       dpi = 600, width = 8, height = 4)

#### Pycnocline depth vs integrated chl ####
yoi <- 2024

totalchl <- ctd_data %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Year == yoi) %>% 
  group_by(Locator, Date) %>% 
  summarize(Int_chl = trapz(Depth, Chlorophyll))

data_to_plot <- strat_data %>% 
  filter(!(Locator %in% c("PENNCOVECW", 
                          "PENNCOVEPNN001", 
                          "PENNCOVEWEST", 
                          "PSUSANBUOY")), 
         Year == yoi) %>% 
  select(Locator, Date, Depth_pycno) %>% 
  left_join(totalchl)

ggplot(data = data_to_plot, 
       aes(x = Depth_pycno, 
           y = Int_chl)) + 
  theme_bw() + 
  geom_point() + 
  facet_wrap(~ Locator, 
             scales = "free", 
             nrow = 2) + 
  labs(x = "Pycnocline depth (m)", 
       y = "1-50 m chlorophyll (mg/L*m)", 
       title = yoi)
ggsave(here("figs", "strat", 
            paste0("pyc_depth_vs_int_chl_", yoi, ".png")), 
       dpi = 600, width = 8, height = 4)

#### Stratification vs integrated chl ####
yoi <- 2024

totalchl <- ctd_data %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Year == yoi) %>% 
  group_by(Locator, Date) %>% 
  summarize(Int_chl = trapz(Depth, Chlorophyll))

data_to_plot <- strat_data %>% 
  filter(!(Locator %in% c("PENNCOVECW", 
                          "PENNCOVEPNN001", 
                          "PENNCOVEWEST", 
                          "PSUSANBUOY")), 
         Year == yoi) %>% 
  select(Locator, Date, Max_buoyfreq) %>% 
  left_join(totalchl)

ggplot(data = data_to_plot, 
       aes(x = Max_buoyfreq, 
           y = Int_chl)) + 
  theme_bw() + 
  geom_point() + 
  facet_wrap(~ Locator, 
             scales = "free", 
             nrow = 2) + 
  labs(x = "Max. buoyancy frequency (cycles/hr)", 
       y = "1-50 m chlorophyll (mg/L*m)", 
       title = yoi)

ggsave(here("figs", "strat", 
            paste0("max_buoyfreq_vs_int_chl_", yoi, ".png")), 
       dpi = 600, width = 8, height = 4)

#### Stratification over the year ####
yoi <- 2024

temp <- strat_data %>% 
  filter(Year == yoi, 
         !(Locator %in% c("PENNCOVECW", 
                          "PENNCOVEPNN001", 
                          "PENNCOVEWEST", 
                          "PSUSANBUOY")))

for (station in unique(temp$Locator)) {
  data_to_plot <- temp %>% 
    filter(Locator == station)
  
  ggplot(data = data_to_plot, 
         aes(x = Date, 
             y = Max_buoyfreq)) + 
    theme_bw() + 
    geom_point() + 
    labs(x = "", 
         y = "Max. buoyancy frequency (cycles/hr)", 
         title = paste(station, yoi)) + 
    scale_x_date(date_breaks = "1 month", 
                 minor_breaks = "1 month", 
                 date_labels = "%b")
  
  ggsave(here("figs", "strat", 
              paste0(station, "_", yoi, "maxbuoyfreq.png")), 
         dpi = 600, height = 4, width = 6)
}

  