###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 109-intext-statistics.R: calculate in text statistics

# load data
ltr2023 <- read_csv(here::here("gen", "lt-timing.csv"))

# Country-level: LT-SB
ltr2023 %>% slice_min(ltr.stb.percent, n = 1) %>% select(country, ltr.stb.percent, ltr.stb.per.lower, ltr.stb.per.upper)
ltr2023 %>% slice_max(ltr.stb.percent, n = 1) %>% select(country, ltr.stb.percent, ltr.stb.per.lower, ltr.stb.per.upper)

highest_ltsb <- ltr2023 %>%
  filter(ltr.stb.percent > 10) %>%
  arrange(desc(ltr.stb.percent))
nrow(highest_ltsb)
highest_ltsb$country    

# Country-level: LT-NM 
ltr2023 %>% slice_min(ltr.nmr.percent, n = 1) %>% select(country, ltr.nmr.percent, ltr.nmr.per.lower, ltr.nmr.per.upper)
ltr2023 %>% slice_max(ltr.nmr.percent, n = 1) %>% select(country, ltr.nmr.percent, ltr.nmr.per.lower, ltr.nmr.per.upper)

highest_ltnm <- ltr2023 %>%
  filter(ltr.nmr.percent > 10) %>%
  arrange(desc(ltr.nmr.percent))
nrow(highest_ltnm)
highest_ltnm$country    

# Country-level: LT-SN 
ltr2023 %>% slice_min(ltr.loss.percent, n = 1) %>% select(country, ltr.loss.percent)
ltr2023 %>% slice_max(ltr.loss.percent, n = 1) %>% select(country, ltr.loss.percent)

highest_ltsn <- ltr2023 %>%
  filter(ltr.loss.percent > 10) %>%
  arrange(desc(ltr.loss.percent))
nrow(highest_ltsn)
highest_ltsn$country  


# Multiples
folds <- ltr2023 %>%
  summarise(
    stb_fold  = signif(max(ltr.stb.percent, na.rm = TRUE) /
                         min(ltr.stb.percent, na.rm = TRUE), 3),
    nm_fold   = signif(max(ltr.nmr.percent, na.rm = TRUE) /
                         min(ltr.nmr.percent, na.rm = TRUE), 3),
    loss_fold = signif(max(ltr.loss.percent, na.rm = TRUE) /
                         min(ltr.loss.percent, na.rm = TRUE), 3)
  )


# Case studies
## Djibouti and Somalia
##  Botswana and Burundi
ltr2023_sub <- ltr2023 %>%
  filter(country %in% c("Djibouti", "Somalia", "Botswana", "Burundi"))

# Contribution of stillbirth to LT-SN
ltr2023 %>% slice_min(prop.stb, n = 5) %>% select(country, prop.stb)

# Max
ltr2023 %>% slice_max(prop.stb, n = 5) %>% select(country, prop.stb)

