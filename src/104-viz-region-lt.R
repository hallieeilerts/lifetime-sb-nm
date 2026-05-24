###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 104-viz-region-lt.R: generate visualizations of region-level LT indicators ---

# set path
here::i_am("src/104-viz-region-lt.R")

# load data
ltrstb <- read_csv("gen/lt_stillbirth_by_sdg_region.csv")
ltrnm <- read_csv("gen/lt_neonatal_mortality_by_sdg_region.csv")

# set sdg regions
sdg_regions <- c(
  "Central and Southern Asia",
  "Eastern and South-Eastern Asia",
  "Europe and Northern America",
  "Latin America and the Caribbean",
  "Northern Africa and Western Asia",
  "Oceania",
  "Sub-Saharan Africa"
)

## ---- Figure 5: LTR region results over time ----

reg_order <- ltrstb %>% 
  filter(region %in% sdg_regions & year == 2023) %>%
  arrange(-ltr_median) %>%
  select(region) %>% pull()
  
plot_ltrreg_trends <- ltrstb %>% 
  mutate(type = "Stillbirth") %>% 
  bind_rows(ltrnm %>% mutate(type = "Neonatal mortality")) %>% 
  filter(region %in% sdg_regions) %>% 
  mutate(region = factor(region, reg_order)) %>%
  ggplot(aes(year, ltr_median*100)) + 
  geom_line(aes(color = region))+
  facet_wrap(~type)+
  geom_ribbon(aes(year, min = ltr_lower*100, max = ltr_upper*100, fill = region),
              alpha = 0.4)+
  labs(y = "Lifetime total (per 100 women)", x = "Year") + 
  scale_color_viridis_d(option = "plasma", begin = 0.15, end = 0.85, name = "Region") +
  scale_fill_viridis_d(option = "plasma", begin = 0.15, end = 0.85, name = "Region") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )
plot_ltrreg_trends

ggsave(here::here("gen/figures", "figure-5_ltr-reg-trends.png"), plot = plot_ltrreg_trends, width = 10, height = 8, dpi = 400, bg = "white")
