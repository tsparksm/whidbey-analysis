#### Setup ####
source(here::here("src", "utility_functions.R"))

data_ctd <- load_composite(0.5, monthly = FALSE) %>% 
  mutate(Basin = "Whidbey")
data_central <- load_composite(0.5, location = "Central", monthly = FALSE) %>% 
  mutate(Basin = "Central")
data_discrete <- load_whidbey_discrete()

good_quals_ctd <- c(NA, "TA")
good_quals_discrete <- 0:2

#### Figure - nitrate CTD profiles by station, month ####
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station), 
         aes(x = Depth, y = NO23, group = Date)) + 
    theme_bw() + 
    geom_line() + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Month) + 
    labs(x = "Depth (m)", 
         y = "Nitrate + nitrite N(mg/L)", 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "no23", 
                         paste0(station, "_NO23.png")), 
         height = 5, width = 5)
}

#### Figure - DO CTD profiles by station, month ####
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = DO, group = Date)) + 
    geom_hline(yintercept = 6, color = "orange") + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Month) + 
    labs(x = "Depth (m)", 
         y = "Dissolved oxygen (mg/L)", 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "do", 
                         paste0(station, "_DO.png")), 
         height = 5, width = 5)
}

#### Figure - chl CTD profiles by station, month ####
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  Depth <= 50)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = Chlorophyll, group = Date)) + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Month) + 
    labs(x = "Depth (m)", 
         y = "Chlorophyll a (mg/L)", 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "chl", 
                         paste0(station, "_chl.png")), 
         height = 5, width = 5)
}

#### Figure - nitrate bottle by station, depth, time ####
for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_discrete %>% 
           filter(ParmId == 14, 
                  QualityId %in% good_quals_discrete, 
                  Locator == station), 
         aes(x = CollectDate, 
             y = Value, 
             shape = grepl("MDL", QfrCode))) + 
    theme_bw() + 
    geom_point() + 
    scale_shape_manual(values = c(16, 1)) + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "Nitrate (mg/L)", 
         title = station, 
         shape = "<MDL")
  ggsave(filename = here("figs", "bottle", "no23", 
                         paste0(station, "_NO23.png")), 
         height = 5, width = 5)
}

#### Figure - N and P bottle for a single deep station, year ####
yoi <- 2022
stations <- c("SARATOGACH")
for (station in stations) {
  ggplot(data = bottle_data %>% 
           filter(Locator == station, 
                  year(CollectDate) == yoi, 
                  ParmId %in% c(14, 15)), 
         aes(x = CollectDate, 
             y = Value, 
             color = Depth, 
             shape = Detect)) + 
    theme_bw() + 
    geom_point() + 
    facet_grid(rows = vars(ParmDisplayName), 
               scales = "free_y") + 
    scale_x_datetime(date_breaks = "1 month", 
                     date_labels = "%b") + 
    labs(x = "", y = "Concentration (mg/L)", 
         title = paste(station, yoi)) + 
    scale_color_gradient(trans = "log",
                         breaks = c(150, 50, 25, 10, 5, 1)) +
    scale_shape_manual(values = c(1, 16))
  ggsave(here("figs", "bottle", 
              paste0("N_P_", station, "_", yoi, ".png")), 
         dpi = 600, height = 4, width = 6)
  
}

#### Figure - N:P by station, depth, time ####
data_to_plot <- data_discrete %>% 
  filter(ParmId %in% 13:15, 
         QualityId %in% good_quals_discrete) %>% 
  mutate(ND = is.na(Value), 
         Value = ifelse(ND, Mdl, Value), 
         Name = case_when(ParmId == 13 ~ "NH3", 
                          ParmId == 14 ~ "NNN", 
                          TRUE ~ "P")) %>% 
  select(CollectDate, Locator, SampleId, DepthBin, Value, Name, Depth, ND) %>% 
  group_by(CollectDate, Locator, DepthBin, Depth, ND, Name) %>% 
  summarize(Value = mean(Value)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Name, 
              values_from = Value) %>% 
  mutate(N = (NNN + NH3)/14.09, 
         P = P/30.97, 
         NtoP = N/P) %>% 
  group_by(CollectDate, Locator, DepthBin) %>% 
  summarize(NtoP = mean(NtoP), 
            N = mean(N), 
            P = mean(P)) %>% 
  ungroup()

for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_to_plot %>% filter(Locator == station), 
         aes(x = CollectDate, 
             y = NtoP, 
             color = N)) + 
    theme_bw() + 
    geom_point() + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "N:P", 
         title = station, 
         color = "DIN (mg/L)")
  ggsave(filename = here("figs", "bottle", "NtoP", 
                         paste0(station, "_NtoP.png")), 
         height = 5, width = 5)
}

#### Figure - phosphate bottle by station, depth, time ####
for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_discrete %>% 
           filter(ParmId == 15, 
                  QualityId %in% good_quals_discrete, 
                  Locator == station), 
         aes(x = CollectDate, 
             y = Value, 
             shape = grepl("MDL", QfrCode))) + 
    theme_bw() + 
    geom_point() + 
    scale_shape_manual(values = c(16, 1)) + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "Phosphate (mg/L)", 
         title = station, 
         shape = "<MDL")
  ggsave(filename = here("figs", "bottle", "po4", 
                         paste0(station, "_PO4.png")), 
         height = 5, width = 5)
}

#### Figure - ammonia bottle by station, depth, time ####
for (station in unique(data_discrete$Locator)) {
  ggplot(data = data_discrete %>% 
           filter(ParmId == 13, 
                  QualityId %in% good_quals_discrete, 
                  Locator == station), 
         aes(x = CollectDate, 
             y = Value, 
             shape = grepl("MDL", QfrCode))) + 
    theme_bw() + 
    geom_point() + 
    scale_shape_manual(values = c(16, 1)) + 
    facet_wrap(~ DepthBin, ncol = 1) + 
    labs(x = "", 
         y = "Ammonia (mg/L)", 
         title = station, 
         shape = "<MDL")
  ggsave(filename = here("figs", "bottle", "nh3", 
                         paste0(station, "_NH3.png")), 
         height = 5, width = 5)
}

#### Figure - chl CTD profiles by station, year ####
yoi <- 2022
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  Depth <= 50, 
                  year(Date) == yoi)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = Chlorophyll)) + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Date) + 
    labs(x = "Depth (m)", 
         y = expression(Chlorophyll~(mu*g/L)), 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "chl", 
                         paste0(station, "_", yoi, "_chl.png")), 
         height = 5, width = 5)
}

#### Figure - surface density CTD profiles by station, year ####
yoi <- 2022
for (station in unique(data_ctd$Locator)) {
  ggplot(data = data_ctd %>% 
           filter(Locator == station, 
                  Depth <= 50, 
                  year(Date) == yoi)) + 
    theme_bw() + 
    geom_line(aes(x = Depth, y = SigmaTheta)) + 
    coord_flip() + 
    scale_x_reverse(expand = c(0, 0)) + 
    facet_wrap(~ Date) + 
    labs(x = "Depth (m)", 
         y = expression(sigma [theta]~(kg/m^3)), 
         title = station)
  ggsave(filename = here("figs", "ctd-profiles", "density", 
                         paste0(station, "_", yoi, "_surface_sigmat.png")), 
         height = 5, width = 5)
}


#### Figure - integrated chl by station, year ####
yoi <- 2022

totalchl <- data_ctd %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Year == yoi, 
         Locator != "PENNCOVEPNN001") %>% 
  group_by(Locator, Date) %>% 
  summarize(Int_chl = trapz(Depth, Chlorophyll))

ggplot(data = totalchl, 
       aes(x = Date, 
           y = Int_chl)) + 
  theme_bw() + 
  geom_point() + 
  facet_wrap(~ Locator) + 
  labs(x = "", 
       y = "1-50 m integrated chl (mg/L*m)", 
       title = yoi)

ggsave(here("figs", paste0(yoi, "_50m_int_chl.png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - integrated chl by year - deep stations basin comparison ####
yoi <- 2022
stations <- c("SARATOGARP", "SARATOGAOP", "SARATOGACH", 
              "PSUSANKP", "PSUSANENT", "Poss DO-2")

totalchl <- data_ctd %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Year == yoi, 
         Locator %in% stations) %>% 
  group_by(Locator, Date) %>% 
  summarize(Int_chl = pracma::trapz(Depth, Chlorophyll))

ggplot(data = totalchl, 
       aes(x = Date, 
           y = Int_chl, 
           color = Locator)) + 
  theme_bw() + 
  geom_point() + 
  geom_line() + 
  labs(x = "", 
       y = "Chlorophyll fluorescence (mg/L*m)", 
       color = "", 
       title = paste(yoi, "1-50 m integrated chlorophyll")) + 
  scale_color_brewer(palette = "Dark2") + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")

ggsave(here("figs", paste0(yoi, "_50m_int_chl_deep.png")), 
       dpi = 600, height = 4, width = 6)


yoi <- 2022
stations <- c("SARATOGARP", "SARATOGAOP", "SARATOGACH", 
              "PSUSANKP", "PSUSANENT", "Poss DO-2", 
              "KSBP01", "JSUR01", "NSEX01", "LSEP01", "LSNT01")

totalchl <- data_ctd %>% 
  full_join(data_central) %>% 
  filter(Depth >= 1, 
         Depth <= 50, 
         Year == yoi, 
         Locator %in% stations) %>% 
  group_by(Basin, Locator, Date) %>% 
  summarize(Int_chl = pracma::trapz(Depth, Chlorophyll))

ggplot(data = totalchl, 
       aes(x = Date, 
           y = Int_chl, 
           color = Basin)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  labs(x = "", 
       y = "Chlorophyll fluorescence (mg/L*m)", 
       color = "", 
       title = paste(yoi, "1-50 m integrated chlorophyll")) + 
  scale_color_brewer(palette = "Paired") + 
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b")

ggsave(here("figs", paste0(yoi, "_50m_int_chl_deep_CvsW.png")), 
       dpi = 600, height = 4, width = 6)

#### Figure - minimum DO by year ####
yoi <- 2022

data_to_plot <- data_ctd %>% 
  filter(Year == yoi, 
         Locator != "PENNCOVEPNN001") %>% 
  group_by(Locator, Date) %>% 
  summarize(MinDO = min(DO))

ggplot(data = data_to_plot, 
       aes(x = Date, 
           y = MinDO)) + 
  theme_bw() + 
  geom_point() + 
  facet_wrap(~ Locator, 
             ncol = 5) + 
  labs(x = "", 
       y = "Minimum DO (mg/L)", 
       color = "", 
       title = yoi) + 
  scale_x_date(date_breaks = "2 month", 
               date_labels = "%b") + 
  scale_y_continuous(breaks = seq(0, 12, by = 2)) + 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4)) + 
  geom_hline(aes(yintercept = 6, color = "WA WQS"), 
             linetype = "dashed") + 
  geom_hline(aes(yintercept = 2, color = "Hypoxia"), 
                 linetype = "dashed") + 
  scale_color_manual(values = c("red", "black"))

ggsave(here("figs", paste0(yoi, "_min_DO.png")), 
       dpi = 600, height = 6, width = 11)
