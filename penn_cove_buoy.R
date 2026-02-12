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
       color = "") + 
  geom_hline(color = "red", 
             linetype = "dashed", 
             yintercept = 2)
ggsave(here("figs", "penncove", 
            paste(yoi, "_surface_vs_bottom_do.png")), 
       height = 6, width = 12)

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


#### Figure: wind arrows - 1 year ####
data_to_plot <- surf_data %>% 
  filter(Year == yoi) %>% 
  mutate(x = WindSpeed_surface*cos((-WindDirection_surface+270)*pi/180), 
         y = WindSpeed_surface*sin((-WindDirection_surface+270)*pi/180)) %>% 
  group_by(Date) %>% 
  summarize(xavg = mean(x, na.rm = TRUE), 
            yavg = mean(y, na.rm = TRUE))

n <- nrow(data_to_plot)

df.sliced <- data_to_plot %>%
  mutate(rown = row_number(), 
         datelabel = paste(month.abb[month(Date)], day(Date))) %>% 
  slice(round(seq(1, n(), by = 7)))

ggplot(data_to_plot, 
            aes(x = 1:n, 
                y = 0,
                xend = 1:n + xavg,
                yend = yavg)) +
  theme_bw() +
  geom_segment(arrow = arrow(angle = 20, length = unit(.03, "npc"), type = "closed")) +
  labs(x = "", 
       y = "Wind velocity (m/s)", 
       title = paste("Penn Cove daily winds -", yoi)) + 
  scale_x_continuous(breaks = df.sliced$rown, 
                     labels = df.sliced$datelabel) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

ggsave(here("figs", "penncove", paste0(yoi, "_penncove_wind.png")), 
       dpi = 600, 
       height = 2, 
       width = 12)

#### Hypoxic time ####
library(DescTools)
temp <- bottom_data %>% 
  filter(Year == yoi, 
         Oxygen_final_bottom == 1, 
         !is.na(Oxygen_bottom)) %>% 
  mutate(Span_start = NA, 
         Span_end = NA)

f_hypoxic <- temp %>% 
  summarize(sum(Oxygen_bottom < 2, na.rm = TRUE) / n()) %>% 
  pull()

current_span <- FALSE
for (i in 1:nrow(temp)) {
  value <- temp$Oxygen_bottom[i]
  if (value >= 2 & !(current_span)) {
    current_span <- FALSE
    temp$Span_start[i] <- NA
    temp$Span_end[i] <- NA
  } else if (value >= 2 & current_span) {
    current_span <- FALSE
    temp$Span_end[i-1] <- temp$DateTime[i-1]
    temp$Span_start[i] <- NA
    temp$Span_end[i] <- NA
  } else if (value < 2 & !(current_span)) {
    current_span <- TRUE
    span_start <- temp$DateTime[i]
    temp$Span_start[i] <- span_start
  } else {
    temp$Span_start[i] <- span_start
  }
}

# To do in future: check for consecutivity

span_temp <- temp %>% 
  group_by(Span_start) %>% 
  summarize(Span_start = as.POSIXct(mean(Span_start), 
                                    origin = "1970-01-01"), 
            Span_end = as.POSIXct(mean(Span_end, na.rm = TRUE), 
                                  origin = "1970-01-01"), 
            Length_m = as.numeric(difftime(Span_end, 
                                           Span_start, 
                                           units = "mins")) + 15, 
            Length_h = Length_m / 60, 
            Length_d = Length_h / 24)

summ_temp <- span_temp %>%
  ungroup() %>% 
  summarize(Mean = mean(Length_d, na.rm = TRUE), 
           Max = max(Length_d, na.rm = TRUE), 
           Median = median(Length_d, na.rm = TRUE), 
           Mode = Mode(Length_d, na.rm = TRUE))
