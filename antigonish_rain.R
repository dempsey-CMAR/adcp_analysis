

rain <- read_csv(
  here("supp_data/antigonish_weather.csv"),
  show_col_types = FALSE,
  col_types = c("D______________n")
  ) %>%
  select(date = Date, rain_in = Precip_Accum_Sum_in)

ggplot(rain, aes(date, rain_in)) +
  geom_point() +
  geom_line()


depls <- c("AT001", "AT002", "AT003", "AT004")

# import Antigonish Harbour deployments
dat_all <- adcp_import_data(here("data"), county = "antigonish") %>%
  filter(deployment_id %in% depls) %>%
  mutate(sea_water_speed_cm_s = sea_water_speed_m_s * 100) %>%
  select(-c(sea_water_speed_m_s, county, waterbody, bin_depth_below_surface_m))

dat_day <- dat_all %>%
  mutate(date = as_date(timestamp_utc)) %>%
  group_by(station, date) %>%
  summarise(mean_current = mean(sea_water_speed_cm_s)) %>%
  ungroup() %>%
  left_join(rain, by = "date") %>%
  na.omit()


speed_rain <- dat_day %>%
  group_by(station) %>%
  summarise(corr = round(cor(mean_current, rain_in), digits = 4))

ggplot(dat_day, aes(mean_current, rain_in)) +
  geom_point() +
  facet_wrap(~station, scales = "free")


# remove 0 rainfall -------------------------------------------------------

dat_day %>%
  filter(rain_in > 0) %>%
  group_by(station) %>%
  summarise(corr = round(cor(mean_current, rain_in), digits = 4))

dat_day %>%
  filter(rain_in > 0) %>%
  ggplot(aes(mean_current, rain_in)) +
  geom_point() +
  facet_wrap(~station, scales = "free")


