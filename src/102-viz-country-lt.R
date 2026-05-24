###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 102-viz-country-lt.R: plot country-level LT indicators on world maps ---

# load data
ltr <- read_csv(here::here("gen", "ltr-agg-country.csv"))
ltr2023 <- subset(ltr, year == 2023)

# set colour palette
my_palette <- paletteer_d("ggsci::default_locuszoom")

# n countries/territories included in estimates
length(unique(ltr2023$ISO)) # 199

# load world map
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  mutate(myiso3 = ifelse(iso_a3 == -99, adm0_a3, iso_a3)) %>% # fix handful of countries that don't have iso_a3 values
  mutate(myiso3 = ifelse(admin == "Somaliland", "SOM", myiso3))

# combine Somalia and Somaliland
somalia_union <- world_map %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(
    myiso3 = "SOM",
    geometry = st_union(geometry)
  )

# keep all other countries unchanged
world_map <- world_map %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_union)

# merge data with world map
worldmap_data <- left_join(world_map, ltr2023, by = c("myiso3" = "ISO"))

## ---- Figure 1: world map of LTR-loss ----

breaks <- c(1, 5, 10, 20, 40)

ltrloss_map <- ggplot() +
  geom_sf(data = worldmap_data) +
  geom_sf(data = worldmap_data, aes(fill = ltr.loss.percent, geometry = geometry)) +
  scale_fill_viridis(
    option = "plasma",
    direction = -1,
    trans = scales::pseudo_log_trans(base = 10),
    name = "LT-SN per 100",
    breaks = breaks,
    labels = scales::label_comma(accuracy = 1),
    na.value = "grey50"
  ) +
  coord_sf(clip = "off") +
  guides(fill = guide_colorbar(reverse = TRUE)) +   
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, -1, "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right"
  )
ltrloss_map

ggsave(here::here("gen/figures", "figure-1_total-loss-map.png"), plot = ltrloss_map, width = 10, height = 5, dpi = 400, bg = "white")




## ---- Figure S1: world map of LTR-STB ----

ltrstb_map <- ggplot() +
  geom_sf(data = worldmap_data, fill = "lightgray") +
  geom_sf(data = worldmap_data, aes(fill = ltr.stb.percent, geometry = geometry)) +
  scale_fill_viridis(
    option = "plasma",
    direction = -1,
    trans = scales::pseudo_log_trans(base = 10),
    name = "LT-SB",
    breaks = breaks,
    labels = scales::label_comma(accuracy = 1)
  ) +
  guides(fill = guide_colorbar(reverse = TRUE)) +   
  coord_sf(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right"
  )

ggsave(here::here("gen/figures", "figure-s1_stb-map.png"), plot = ltrstb_map, width = 10, height = 5, dpi = 400, bg = "white")


## ---- Figure S2: world map of LTR-NM ----

ltrnmr_map <- ggplot() +
  geom_sf(data = worldmap_data, fill = "lightgray") +
  geom_sf(data = worldmap_data, aes(fill = ltr.nmr.percent, geometry = geometry)) +
  scale_fill_viridis(
    option = "plasma",
    direction = -1,
    trans = scales::pseudo_log_trans(base = 10),
    name = "LT-NM",
    breaks = breaks,
    labels = scales::label_comma(accuracy = 1)
  ) +
  coord_sf(clip = "off") +
  guides(fill = guide_colorbar(reverse = TRUE)) +   
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right"
  )

ggsave(here::here("gen/figures", "figure-s2_nmr-map.png"), plot = ltrnmr_map, width = 10, height = 5, dpi = 400, bg = "white")


