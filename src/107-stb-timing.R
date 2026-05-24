###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 107-stb-timing.R: assess contribution of ante- and intrapartum stb to ltr-stb ---

# load LTR data
ltr <- read_csv(here::here("gen", "lt-agg-country.csv"))
ltr_data <- subset(ltr, year == 2023)

# load intrapartum proportion
intra <- read_csv(here::here("data", "ipsb_by_country_2025-03-21.csv"))
intra <- intra %>% 
  rename(prop.intra = q50)

# merge LTR data with intrapartum proportion
ltr_data <- left_join(ltr_data, intra, by = c("ISO" = "iso", "year"))

# calculate proportion of LTR-STB from intrapartum vs antepartum
ltr_data <- ltr_data %>% 
  mutate(ltr.intra.percent = (ltr.stb.percent * prop.intra), 
         ltr.ante.percent = (ltr.stb.percent * (1-prop.intra)))


# assign transition stage
ltr_data <- ltr_data %>%
  mutate(total.rate = stb.rate + nmr.rate,
         transition = case_when(
           total.rate >= 80                    ~ "1",
           total.rate >= 55 & total.rate < 80  ~ "2",
           total.rate >= 30 & total.rate < 55  ~ "3",
           total.rate >= 15 & total.rate < 30  ~ "4",
           total.rate < 15                      ~ "5"
         ),
         transition = factor(transition, levels = c("1","2","3","4","5"),
                             labels = c("1","2","3","4","5")))

write_csv(ltr_data, here::here("gen", "lt-timing.csv"))


## ---- Figure 4: contribution of intrapartum stb to ltr-stb by mortality transition stage ----

# fit same trend line from the plot
fit <- loess(ltr.stb ~ prop.intra, data = ltr_data)
ltr_data$residual <- residuals(fit)
# Label points far above or below the trend line
outliers <- ltr_data %>%
  filter(abs(residual) > quantile(abs(residual), 0.9)) 
# Drop and relabel some so plot isn't too cluttered
outliers <- outliers %>%
  filter(!(country %in% c("Turkmenistan", "Togo", "Myanmar"))) %>%
  mutate(label = country) %>%
  mutate(label = case_when(
    country == "United Republic of Tanzania" ~ "Tanzania",
    country == "Democratic Republic of the Congo" ~ "DRC",
    country == "Central African Republic" ~ "Central\nAfrica\nRepublic",
    TRUE ~ label
  ))

# plot: color by transition, overall LOESS in black
plot_timing <- ggplot(ltr_data, aes(x = prop.intra, y = ltr.stb, color = transition)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "black", linetype = "dashed", size = 0.8) +
  geom_text_repel(
    data = outliers,
    aes(label = label),
    color = "black",
    size = 3.2,
    box.padding = 0.5,
    point.padding = 0.3,
    max.overlaps = Inf,       # never drop labels
    show.legend = FALSE
  ) +
  labs(
    x = "Intrapartum stillbirth (of total stillbirths)",
    y = "LT-SB",
    color = "Transition stage",
    #title = "Contribution of intrapartum stillbirth by overall lifetime stillbirths"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "plasma", begin = 0.15, end = 0.85) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold")
  )
plot_timing

# Save
ggsave(here::here("gen/figures", "figure-4_stb-timing.png"), plot = plot_timing, width = 8, height = 6, dpi = 400, bg = "white")

## ---- Table S3: lt-sb by antepartum or intrapartum timing  ----

gt_tblS3 <- ltr_data %>%
  arrange(region, country) %>% 
  select(region, country, prop.intra, ltr.stb.percent, 
         ltr.intra.percent, ltr.ante.percent) %>% 
  mutate(prop.intra = prop.intra *100) %>% 
  mutate(across(c(prop.intra, ltr.stb.percent, ltr.intra.percent, ltr.ante.percent), round, 1)) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Global estimates of the Lifetime Stillbirths by Timing, 2023") %>% 
  cols_label(
    region = md("Region"), 
    country = md("Country"), 
    prop.intra = md("Intrapartum SB (%)"),
    ltr.stb.percent = md("LT-SB"), 
    ltr.intra.percent = md("LT Intrapartum SB"), 
    ltr.ante.percent = md("LT Antepartum SB"),
  ) %>% 
  tab_options(row_group.as_column = TRUE, table.font.size = 14)

# save table
gtsave(gt_tblS3, here::here("gen/tables", "table-S3_stb_timing.docx"))


