#### Setup ####
source(here::here("src", "utility_functions.R"))
library(RSocrata)

phyto_data <- get_phyto()
group_data <- get_phyto_group()
discrete_data <- load_whidbey_discrete()

whidbey_stations <- c(
  "PENNCOVEENT", 
  "PENNCOVECW", 
  "PENNCOVEWEST", 
  "PSUSANBUOY", 
  "SARATOGACH"
)

#### Whidbey total biovolume over time ####
phyto_data %>% 
  filter(locator %in% whidbey_stations) %>% 
  ggplot(aes(x = collect_date, y = total_biovolume)) + 
  geom_point() + 
  facet_wrap(~locator)

#### Whidbey vs Central for 1 year ####
locators <- c(
  "SARATOGACH", 
  "PENNCOVEENT", 
  "PSUSANBUOY", 
  "KSBP01", 
  "NSAJ02", 
  "LTED04" 
)

yoi <- 2022

data_to_plot <- phyto_data %>% 
  filter(locator %in% locators, 
         year(collect_date) == yoi) %>% 
  mutate(Basin = ifelse(locator %in% locators[1:3], 
                        "Whidbey", 
                        "Central"))

data_to_plot$locator <- factor(
  x = data_to_plot$locator, 
  levels = locators
)

ggplot(data_to_plot, 
       aes(x = collect_date, y = total_biovolume)) + 
  geom_point() + 
  facet_wrap(~ locator + Basin, 
             labeller = labeller(.multi_line = FALSE)) + 
  theme_bw() + 
  scale_x_datetime(date_breaks = "1 month", 
                   date_labels = "%b") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) + 
  labs(x = "", y = expression(Total~biovolume~(mm^3/L)))
ggsave(here("figs", "phyto", 
            paste0("Central_vs_Whidbey_biovolume_", yoi, ".png")), 
       dpi = 600, height = 4, width = 6)

#### Biovolume and chlorophyll ####
temp <-  phyto_data %>% 
  select(c(sample_number, total_abundance, total_biovolume)) %>% 
  rename(LabSampleNum = sample_number)
combined_data <- discrete_data %>% 
  filter(ParmId == 1) %>% 
  left_join(temp)

ggplot(combined_data %>% 
         filter(DepthBin == "surface"), 
       aes(x = CollectDate)) + 
  geom_point(aes(y = Value)) + 
  geom_point(aes(y = total_biovolume), color = "gray")

ggplot(combined_data %>% 
         filter(DepthBin == "surface"), 
       aes(x = Value, y = total_biovolume)) + 
  geom_point()

#### Biovolume and nutrients ####
temp <-  phyto_data %>% 
  select(c(sample_number, total_abundance, total_biovolume)) %>% 
  rename(LabSampleNum = sample_number)
combined_data <- discrete_data %>% 
  filter(ParmId == 14) %>% 
  left_join(temp)

ggplot(combined_data %>% 
         filter(DepthBin == "surface"), 
       aes(x = total_biovolume, 
           y = Value, 
           color = as.factor(month(CollectDate)))) + 
  geom_point() + 
  facet_wrap(~ Locator)
