#### Setup ####
source(here::here("src", "utility_functions.R"))

data_whidbey <- load_composite(bin_size = 0.5, 
                               monthly = FALSE, 
                               location = "Whidbey") %>% 
  mutate(Agency = "KC")

data_ecology <- load_EIM_continuous("PNN001") %>% 
  mutate(Agency = "ECY")

#### Combine datasets ####
data_combined <- data_whidbey %>% 
  filter(Locator == "PENNCOVEENT") %>% 
  full_join(data_ecology) %>% 
  mutate(Year = ifelse(is.na(Year), year(Date), Year), 
         Month = ifelse(is.na(Month), month(Date), Month))

#### DO profiles by year ####
for (yoi in unique(data_combined$Year)) {
  data_combined %>% 
    filter(Year == yoi) %>% 
    ggplot(aes(x = Depth, y = DO)) + 
    coord_flip() + 
    geom_line() + 
    facet_wrap(~ Date) + 
    theme_bw() + 
    labs(x = "Depth (m)", y = "DO (mg/L)") + 
    scale_x_reverse()
  ggsave(here("figs", "ctd-profiles", "do", "ecy-comp", paste0(yoi, "_DO.png")), 
         dpi = 600, height = 5, width = 5)
}


