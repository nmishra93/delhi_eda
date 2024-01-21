library(sf)
library(tidyverse)
library(here)
library(scico)


dmrc_linecolors <-
  c(
    "Delhi Metro Yellow Line"  = "#FFDF00",
    "Delhi Metro Violet Line"  = "#553592",
    "Delhi Metro Red Line"     = "#FF4040",
    "Rapid Metro Line"         = "#20B2AA",
    "Delhi Metro Pink Line"    = "#FC8EAC",
    "Delhi Metro Magenta Line" = "#CC338B",
    "Delhi Metro Green Line"   = "#6DBD72",
    "Delhi Metro Blue Line"    = "#4169E1",
    "Orange Line"              = "#FF8C00"
  )

ncr_lines <-
  st_read(
    here::here(
      "delhi_ncr_current_dmrc.geojson"
    )
  )

igi_airport <-
  st_read(
    here::here(
      "osm_igi_airport.geojson"
    )
  )

dmrc_lines <-
  st_read("delhi-metro-lines.kml") |>
  # drop z dimension
  st_zm()


delhi_admin <-
  st_read(here::here(
    "delhi_1997-2012_district.json"
  ))

delhi_wards <-
  st_read(here::here(
    "Delhi_Ward_Boundary_2022"
  ))

delhi_admin |>
  # plot
  ggplot() +
  geom_sf() +
  theme_minimal()


aqi_data <-
  read_csv(
    here::here(
      "data_gov_realtime_aqi_api_20240120_222912.csv"
    )
  ) |>
  filter(state == "Delhi",
         pollutant_id == "PM10") |>
  select(station, longitude, latitude, pollutant_avg) |>
  drop_na()

points_df <-
  aqi_data |>
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(delhi_admin))

joined_df <-
  st_join(delhi_admin, points_df)

summarized_df <-
  joined_df |>
  group_by(DISTRICT) |>
  summarise(avg_pm10 = mean(pollutant_avg))

ggplot() +
  geom_sf(data = delhi_admin) +
  geom_sf(data = points_df, aes(fill = pollutant_avg),
          shape = 21) +
  theme_minimal()

ggplot() +
  geom_sf(data = delhi_admin) +
  geom_sf(data = summarized_df, aes(fill = avg_pm10)) +
  scale_fill_scico(palette = "lajolla",
                   midpoint = 250,
                   direction = -1) +
  labs(fill = "PM10 (ug/m3)") +
  guides(
    fill = guide_colorsteps(
      barheight = unit(0.2, "cm"),
      barwidth = unit(3, "cm"),
      title.position = "left",
      title.vjust = 1,
      title.theme = element_text(size = 8)

    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    panel.background = element_rect(fill = "white",
                                    color = NA),
    plot.background = element_rect(fill = "white",
                                   color = NA)
  )

ggsave(
  here::here("delhi_pm10.png"),
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


ncr_lines |>
  ggplot() +
  geom_sf(
    fill = "gray97",
    color = "gray70",
    linewidth = 0.1
  ) +
  geom_sf(
    data = igi_airport,
    inherit.aes = F,
    color = "black",
    fill = "gray97"
  ) +
  geom_sf(
    data = points_df,
    aes(fill = pollutant_avg),
    shape = 21,
    stroke = 0.2,
    color = "gray20",
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_sf(
    data = dmrc_lines,
    aes(color = Name),
    inherit.aes = F) +
  scale_color_manual(
    values = dmrc_linecolors
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave(
  here::here("dmrc_ncr.png"),
  width = 6,
  height = 6,
  units = "in",
  dpi = 300
)


