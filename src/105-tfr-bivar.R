###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 105-tfr-bivar.R: bivariate analysis of TFR and LT indicators ---

# load data
ltr <- read_csv(here::here("gen", "lt-agg-country.csv"))
ltr2023 <- subset(ltr, year == 2023)

# n countries/territories included in estimates
length(unique(ltr2023$ISO)) # 199

# load world map
world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  mutate(myiso3 = ifelse(iso_a3 == -99, adm0_a3, iso_a3)) # fix handful of countries that don't have iso_a3 values

# combine Somalia and Somaliland
somalia_union <- world_map %>%
  filter(admin %in% c("Somalia", "Somaliland")) %>%
  summarise(
    admin = "Somalia",
    continent = "Africa",
    myiso3 = "SOM",
    geometry = st_union(geometry),
    .groups = "drop"
  )

# keep all other countries unchanged
world_map <- world_map %>%
  filter(!admin %in% c("Somalia", "Somaliland")) %>%
  bind_rows(somalia_union)

# merge data with world map
worldmap_data <- left_join(world_map, ltr2023, by = c("myiso3" = "ISO"))

# subset Africa
africamap_data <- filter(world_map, continent == "Africa")

# calculate TFR quantiles for sub-Saharan Africa
brks <- classInt::classIntervals(
  ltr2023$tfr[ltr2023$region == "Sub-Saharan Africa"],
  n = 3,
  style = "quantile"
)$brks

# group TFR by quantiles
ssa_quantiles <- ltr2023 %>%
  filter(region == "Sub-Saharan Africa") %>%
  mutate(
    tfr_tertile = cut(
      tfr,
      breaks = brks,
      include.lowest = TRUE,
      labels = c("Low", "Medium", "High")
    )
  )

# categorize countries by tfr and ltr.stb or ltr.nm (bivariate analysis)
bivar_africa <- ltr2023 %>%
  filter(region == "Sub-Saharan Africa") %>%
  select(ISO, tfr, ltr.stb, ltr.nmr, ltr.loss) %>%
  pivot_longer(cols = c(ltr.stb, ltr.nmr), names_to = "ind") %>%
  mutate(ind = factor(ind, levels = c("ltr.stb","ltr.nmr"),
                      labels = c("LT-SB", "LT-NM"))) %>%
  group_by(ind) %>%
  group_modify(~ bi_class(.x, x = value, y = tfr, style = "quantile", dim = 3)) %>%
  ungroup()

# count countries that switch color shades from ltr-stb to ltr-nm
v_blue_stb <- subset(bivar_africa, ind == "LT-SB" & bi_class %in% c("1-2", "1-3", "2-3"))$ISO # 2-3 column 2, row 3
v_red_stb <- subset(bivar_africa, ind == "LT-SB" & bi_class %in% c("2-1", "3-2", "3-1"))$ISO
v_purple_stb <- subset(bivar_africa, ind == "LT-SB" & bi_class %in% c("1-1", "2-2", "3-3"))$ISO
v_blue_nm <- subset(bivar_africa, ind == "LT-NM" & bi_class %in% c("1-2", "1-3", "2-3"))$ISO
v_red_nm <- subset(bivar_africa, ind == "LT-NM" & bi_class %in% c("2-1", "3-2", "3-1"))$ISO
v_purple_nm <- subset(bivar_africa, ind == "LT-NM" & bi_class %in% c("1-1", "2-2", "3-3"))$ISO
length(intersect(v_purple_stb, v_purple_nm)) # 30
length(intersect(v_blue_stb, v_blue_nm)) # 4
length(intersect(v_red_stb, v_red_nm)) # 4
length(intersect(v_purple_stb, v_purple_nm))/length(unique(v_purple_stb, v_purple_nm)) # 0.8823
length(intersect(v_blue_stb, v_blue_nm))/length(unique(v_blue_stb, v_blue_nm)) # 0.5714
length(intersect(v_red_stb, v_red_nm))/length(unique(v_red_stb, v_red_nm)) # 0.5714
sum(v_blue_stb %in% v_blue_nm, v_red_stb %in% v_red_nm, v_purple_stb %in% v_purple_nm)/(length(v_blue_stb) +length(v_red_stb) + length(v_purple_stb)) # 0.7916667

## ---- Figure 2: Africa map TFR by LTR-STB and LTR-NM ----

africamap_bivar <- left_join(bivar_africa, africamap_data, by = c("ISO" = "myiso3"))

plot_africamap_bivar <- ggplot() +
  geom_sf(data = africamap_data, fill = "grey50") +
  geom_sf(data = africamap_bivar, aes(fill = bi_class, geometry = geometry)) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  labs(fill = "Lifetime risk") +
  coord_sf(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, -1, 0, "cm"), # remove some of lower margin
    aspect.ratio = 1,
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(~ind)

legend <- bi_legend(pal = "DkViolet", dim = 3,
                    xlab = "LT",
                    ylab = "TFR",
                    size = 10) +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

plot_africamap_bivar <- ggdraw() +
  draw_plot(plot_africamap_bivar, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.01, 0.12, 0.6)

ggsave(here::here("gen/figures", "figure-2_africa_ltr_tfr.png"), plot = plot_africamap_bivar, width = 10, height = 4, dpi = 400, bg = "white")
