#### Setup ####
source(here::here("src", "utility_functions.R"))
library(ggbreak)

data_whidbey <- load_composite(bin_size = 0.5, 
                               monthly = FALSE, 
                               location = "Whidbey") %>% 
  mutate(Agency = "KC")

data_discrete <- load_whidbey_discrete()

data_ecology <- load_EIM_continuous("PNN001") %>% 
  mutate(Agency = "ECY")

data_ecology_nuts <- load_ecology_nutrients() %>% 
  mutate(Agency = "ECY", 
         NO23 = (NO2_Lab + NO3_Lab)*14.01/1000, 
         PO4 = PO4_Lab*30.97/1000, 
         SiO2 = SiOH4_Lab*60.08/1000) %>% 
  rename(CollectDate = DateCollected, 
         Depth = SamplingDepth, 
         Locator = Station) %>% 
  filter(!is.na(Depth), 
         Locator == "PNN001") %>% 
  select(Locator, CollectDate, Depth, Agency, NO23, PO4, SiO2)

#### Combine datasets ####
data_combined_ctd <- data_whidbey %>% 
  filter(Locator == "PENNCOVEENT") %>% 
  full_join(data_ecology) %>% 
  mutate(Year = ifelse(is.na(Year), year(Date), Year), 
         Month = ifelse(is.na(Month), month(Date), Month))

data_combined_discrete <- data_discrete %>% 
  filter(
    Locator %in% c("PENNCOVEPNN001", "PENNCOVEENT"), 
    ParmId %in% c(14, 15, 21)
  ) %>% 
  mutate(
    ParmDisplayName = case_when(
      ParmId == 14 ~ "NO23", 
      ParmId == 15 ~ "PO4", 
      TRUE ~ "SiO2"
    ), 
    Agency = "KC", 
  ) %>% 
  select(
    Locator, 
    CollectDate, 
    Depth, 
    LabSampleNum, 
    Agency, 
    ParmDisplayName, 
    Value, 
    Detect
  ) %>% 
  pivot_wider(
    names_from = ParmDisplayName, 
    names_sep = "_", 
    values_from = c(Value, Detect)
  ) %>% 
  rename(
    NO23 = Value_NO23, 
    PO4 = Value_PO4, 
    SiO2 = Value_SiO2
  ) %>% 
  full_join(data_ecology_nuts) %>% 
  mutate(
    CollectDate = as.Date(CollectDate), 
    DepthBin = case_when(
      Depth < 2 ~ "surface", 
      Depth > 20 ~ "bottom", 
      TRUE ~ "garbage"
    ), 
    Detect_NO23 = ifelse(
      is.na(Detect_NO23), 
      TRUE, 
      Detect_NO23
    ), 
    Detect_PO4 = ifelse(
      is.na(Detect_PO4), 
      TRUE, 
      Detect_PO4
    ), 
    Detect_SiO2 = ifelse(
      is.na(Detect_SiO2), 
      TRUE, 
      Detect_SiO2
    ), 
    FakeDate = CollectDate
  )
year(data_combined_discrete$FakeDate) <- 2020

#### DO profiles by year ####
for (yoi in unique(data_combined_ctd$Year)) {
  data_combined_ctd %>% 
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

#### Bottom NO3 over time ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "bottom"), 
  aes(
    x = CollectDate, 
    y = NO23, 
    shape = Detect_NO23, 
    color = Agency
  )
) + 
  theme_bw() + 
  geom_point() + 
  scale_wrap(n = 2) + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))

#### Bottom NO3 by FakeDate ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "bottom"), 
  aes(
    x = FakeDate, 
    y = NO23, 
    shape = Detect_NO23, 
    color = as.factor(year(CollectDate))
  )
) + 
  theme_bw() + 
  geom_point(size = 3) + 
  geom_line(linetype = 2) + 
  scale_color_viridis_d(begin = 0, 
                        end = 0.9) + 
  labs(
    x = "", 
    y = "Nitrate (mg N/L)", 
    shape = "", 
    color = "Year"
  )

#### Bottom NO3 by year ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "bottom"), 
  aes(x = FakeDate, 
      y = NO23, 
      shape = Detect_NO23, 
      color = Agency)
) + 
  geom_point(size = 2) + 
  theme_bw() + 
  facet_wrap(~ year(CollectDate), ncol = 1) + 
  labs(
    x = "", 
    y = "Nitrate + nitrite (mg N/L)", 
    shape = "", 
    color = ""
  ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  guides(shape = "none") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))
ggsave(here("figs", "bottle", "no23", "ecy-comp", 
            "PENNCOVEENT_bottom_N_by_year.png"), 
       dpi = 600, height = 6, width = 4)

#### Surface NO3 by year ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "surface"), 
  aes(x = FakeDate, 
      y = NO23, 
      shape = Detect_NO23, 
      color = Agency)
) + 
  geom_point() + 
  theme_bw() + 
  facet_wrap(~ year(CollectDate), ncol = 1) + 
  labs(
    x = "", 
    y = "Nitrate (mg N/L)", 
    shape = "", 
    color = ""
  ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  guides(shape = "none")

#### Bottom PO4 by year ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "bottom"), 
  aes(x = FakeDate, 
      y = PO4, 
      shape = Detect_PO4, 
      color = Agency)
) + 
  geom_point() + 
  theme_bw() + 
  facet_wrap(~ year(CollectDate), ncol = 1) + 
  labs(
    x = "", 
    y = "Phosphate (mg P/L)", 
    shape = "", 
    color = ""
  ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  guides(shape = "none")

#### Surface PO4 by year ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "surface"), 
  aes(x = FakeDate, 
      y = PO4, 
      shape = Detect_PO4, 
      color = Agency)
) + 
  geom_point() + 
  theme_bw() + 
  facet_wrap(~ year(CollectDate), ncol = 1) + 
  labs(
    x = "", 
    y = "Phosphate (mg P/L)", 
    shape = "", 
    color = ""
  ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  guides(shape = "none")

#### Bottom Si by year ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "bottom"), 
  aes(x = FakeDate, 
      y = SiO2, 
      shape = Detect_SiO2, 
      color = Agency)
) + 
  geom_point() + 
  theme_bw() + 
  facet_wrap(~ year(CollectDate), ncol = 1) + 
  labs(
    x = "", 
    y = expression(SiO[2]~(mg/L)), 
    shape = "", 
    color = ""
  ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  guides(shape = "none")

#### Surface Si by year ####
ggplot(
  data = data_combined_discrete %>% 
    filter(DepthBin == "surface"), 
  aes(x = FakeDate, 
      y = SiO2, 
      shape = Detect_SiO2, 
      color = Agency)
) + 
  geom_point() + 
  theme_bw() + 
  facet_wrap(~ year(CollectDate), ncol = 1) + 
  labs(
    x = "", 
    y = expression(SiO[2]~(mg/L)), 
    shape = "", 
    color = ""
  ) + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  guides(shape = "none")
