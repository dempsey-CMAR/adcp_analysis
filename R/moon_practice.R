library(readr)
library(ggplot2)
library(gggibbous)
library(dplyr)

moon_phase <- read_csv(
  here("supp_data/2022_antigonish_moon_phases.csv"),
  show_col_types = FALSE
) %>%
  select(-c(Lunation, Duration)) %>%
  pivot_longer(cols = everything(), names_to = "phase", values_to = "date") %>%
  mutate(
    date = paste0(date, "-2022"),
    date = as_datetime(date, format = "%d-%b-%Y"),
    percent = case_when(
      phase == "New Moon" ~ 0,
      phase == "First Quarter" ~ 0.5,
      phase ==  "Third Quarter" ~ 0.5,
      phase == "Full Moon" ~ 1
    )
  ) %>%
  filter(
    date >= as_datetime("2022-06-07"),
    date <= as_datetime("2022-07-25")
  )


ggplot(moon_phase, aes(date, 12)) +
  geom_moon(ratio = 1, size = 5, fill = "black") +
  geom_moon(
    data = moon_phase, aes(ratio = percent),
    size = 5, right = FALSE
    )

extreme_current %>%
  filter(deployment_id == "AT001") %>%
  ggplot(aes(timestamp_utc, sea_water_speed_cm_s)) +
  annotate(
    "rect", fill = "grey80",
    xmin = as_datetime(-Inf),
    xmax = as_datetime(Inf),
    ymin = 20, ymax = Inf
  ) +
  geom_line(col = pal) +
  geom_moon(
    data = moon_phase,
    aes(date, 32),
    ratio = 1, size = 6, fill = "black") +
  geom_moon(
    data = moon_phase,
    aes(date, 32, ratio = percent),
    size = 6, right = FALSE, fill = "gold"
  )
