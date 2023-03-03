#### Setup ####
source(here::here("src", "utility_functions.R"))

data_whidbey <- load_composite(bin_size = 0.5, 
                               monthly = FALSE, 
                               location = "Whidbey")
data_central <- load_composite(bin_size = 0.5, 
                               monthly = FALSE, 
                               location = "Central")

bottle_data <- load_whidbey_discrete()

good_quals_ctd <- c(NA, "TA")

#### Combine data from Whidbey and Central Basins ####
data_combine <- full_join(data_whidbey, data_central)

#### Figure - compare shallow stations over a year, N and P ####
stations <- c("PENNCOVEWEST", "PENNCOVEENT", "PSUSANBUOY")
yoi <- 2022

ggplot(data = bottle_data %>% 
         filter(Locator %in% stations, 
                year(CollectDate) == yoi, 
                ParmId %in% c(14, 15)), 
       aes(x = CollectDate, 
           y = Value, 
           color = Depth, 
           shape = Detect)) + 
  theme_bw() + 
  geom_point() + 
  facet_grid(cols = vars(Locator), 
             rows = vars(ParmDisplayName), 
             scales = "free_y") + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  labs(x = "", y = "Concentration (mg/L)") + 
  scale_color_gradient(trans = "log",
                       breaks = c(25, 10, 5, 1)) +
  scale_shape_manual(values = c(1, 16))
ggsave(here("figs", "bottle", 
            paste0("N_P_shallow_", yoi, ".png")), 
       dpi = 600, height = 5, width = 8)
