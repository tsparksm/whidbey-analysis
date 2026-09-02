# Setup -------------------------------------------------------------------

library(tidyverse)
source(here::here("src", "utility_functions.R"))

good_quals_ctd <- c(NA, "TA")
good_quals_discrete <- 0:2

date_span <- as.Date(c("2025-09-01", "2026-07-31"))


# Load data ---------------------------------------------------------------

data_discrete <- load_whidbey_discrete() |>
  mutate(
    YearGroup = ifelse(
      between(as.Date(CollectDate), date_span[1], date_span[2]),
      "Year 4",
      "Years 1-3"
    )
  ) |> 
  arrange(desc(YearGroup))

year(data_discrete$FakeDate) <- ifelse(
  month(data_discrete$FakeDate) >= 9,
  2019,
  2020
)

# Figure - Penn Cove nutrients --------------------------------------------

data_discrete |>
  filter(
    Locator == "PENNCOVEENT", 
    ParmId %in% c(14, 15, 21),
    DepthBin %in% c("surface", "25 m")
  ) |> 
  ggplot(aes(x = FakeDate, y = Value, color = YearGroup, shape = Detect)) + 
  theme_bw() + 
  geom_point() + 
  facet_grid(
    cols = vars(DepthBin), 
    rows = vars(ParmDisplayName), 
    scales = "free_y", 
    labeller = label_wrap_gen(width = 20)
  ) + 
  scale_color_manual(values = c("black", "gray")) + 
  scale_shape_manual(values = c(1, 16)) + 
  scale_x_date(
    date_labels = "%b", 
    date_breaks = "3 months", 
    date_minor_breaks = "1 month"
  ) + 
  labs(x = "", y = "mg/L", color = "")

ggsave(
  here("figs", "tech-memo", "year4", "penn_cove_n.png"),
  dpi = 600,
  height = 4,
  width = 6
)
