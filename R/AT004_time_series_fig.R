library(adcp)
library(data.table)
library(dplyr)
library(ggplot2)
library(here)
library(RColorBrewer)

theme_set(theme_light())
pal <- brewer.pal(9, "Blues")[5]

# import Gooseberry Island deployment
dat_raw <- adcp_import_data(here("data"), county = "antigonish") %>%
  filter(deployment_id == "AT004") %>%
  mutate(sea_water_speed_cm_s = sea_water_speed_m_s * 100) %>%
  select(-sea_water_speed_m_s)

# find closest to 2 m above sea floor
bin_height <- dat_raw %>%
  distinct(bin_height_above_sea_floor_m) %>%
  mutate(diff = abs(2 - bin_height_above_sea_floor_m)) %>%
  arrange(diff)

p <- dat_raw %>%
  filter(bin_height_above_sea_floor_m == 1.92)  %>%
  adcp_plot_current_speed_time() +
  geom_line(col = pal, linewidth = 1) +
  scale_x_datetime(date_labels = "%Y-%B-%d")


ggsave(
  p,
  filename = here("figures/gooseberry_island_1.92m.png"),
  units = "cm",
  width = 15, height = 9,
  dpi = 600)


dat_raw %>%
  filter(!(bin_height_above_sea_floor_m %in% c(3.67, 3.92))) %>%
  ggplot(aes(timestamp_utc, sea_water_speed_cm_s)) +
  geom_line(col = pal) +
  facet_wrap(~bin_height_above_sea_floor_m, ncol = 1) +
  scale_x_datetime(date_labels = "%Y-%B-%d")

ggsave(
  filename = here("figures/gooseberry_island_facet.png"),
  units = "cm",
  width = 15, height = 25,
  dpi = 600)
