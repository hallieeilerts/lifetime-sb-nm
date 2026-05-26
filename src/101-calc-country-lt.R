###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 101-calc-country-lt.R: calculate country-level results and create table

# set path
here::i_am("src/101-calc-country-lt.R")

# load available UNWPP country codes
unwpp.codes <- read_csv(here::here("data", "WPP2024_Demographic_Indicators_Medium.csv")) %>%
  pull(ISO3_code) %>% unique()

# read and reshape country-level stillbirth data (UNICEF)
stb_data <- read_csv(here::here("data", "unicef-stillbirth-country-2024.csv")) %>% 
  filter(ISO.Code %in% unwpp.codes) %>%
  pivot_longer(
    cols = `2000.5`:`2023.5`,
    names_to = "YR",
    values_to = "SBR"
  ) %>%
  mutate(
    YR = floor(as.numeric(YR)), 
    ISO = as.character(ISO.Code)
  ) %>%
  select(ISO, SDG.Region, Country.Name, UNICEF.Region, `Uncertainty.Bounds*`, YR, SBR) %>%
  pivot_wider(
    names_from = `Uncertainty.Bounds*`,
    values_from = SBR
  ) %>%
  rename(
    stb.rate = Median,
    stb.lower = Lower,
    stb.upper = Upper
  )

# read and reshape country-level neonatal mortality data (UNICEF)
nmr_data <- read_csv(here::here("data", "unicef-neonatal-mortality-country-2024.csv")) %>% 
  filter(ISO.Code %in% unwpp.codes) %>%
  pivot_longer(
    cols = `2000.5`:`2023.5`,
    names_to = "YR",
    values_to = "NMR"
  ) %>%
  mutate(
    YR = floor(as.numeric(YR)), 
    ISO = as.character(ISO.Code)
  ) %>%
  select(ISO, `Uncertainty.Bounds*`, YR, NMR) %>%
  pivot_wider(
    names_from = `Uncertainty.Bounds*`,
    values_from = NMR
  ) %>%
  rename(
    nmr.rate = Median,
    nmr.lower = Lower,
    nmr.upper = Upper
  )

# merge stb and nmr data
unicef_data <- left_join(stb_data, nmr_data, by = c("ISO", "YR"))

# load WPP life tables and indicators 
WPP.combined <- read_csv(here::here("data", "WPP2024_Life_Table_Abridged_Medium_1950-2023.csv")) %>%
   filter(!is.na(ISO3_code), Sex == "Female", AgeGrp == "15-19") %>%
   select(ISO3_code, Time, lx) %>%
   left_join(
     read_csv(here::here("data", "WPP2024_Demographic_Indicators_Medium.csv")) %>% 
       select(ISO3_code, Time, SRB, NRR, TFR),
     by = c("ISO3_code", "Time"))

# main analysis
make_country_rows <- function(x) {
  CNTRY <- x$ISO
  YR <- x$YR
  region <- x$SDG.Region
  country <- x$Country.Name
  stb.rate <- x$stb.rate
  stb.rate.lower <- x$stb.lower
  stb.rate.upper <- x$stb.upper
  nmr.rate <- x$nmr.rate
  nmr.rate.lower <- x$nmr.lower
  nmr.rate.upper <- x$nmr.upper
  
  message("Processing ", CNTRY, " ", YR)
  
  CNTRY.data <- WPP.combined %>%
    filter(ISO3_code == CNTRY, Time == YR)
  
  l15 <- CNTRY.data$lx
  srb <- 1 + CNTRY.data$SRB / 100
  nrr <- CNTRY.data$NRR
  tfr <- CNTRY.data$TFR
  
  l0 <- 100000
  tb.adj <- 1 / (1 - (stb.rate / 1000))
  
  ltr.stb <- stb.rate * srb * nrr * tb.adj * (l0 / l15) / 1000
  ltr.stb.lower <- stb.rate.lower * srb * nrr * tb.adj * (l0 / l15) / 1000
  ltr.stb.upper <- stb.rate.upper * srb * nrr * tb.adj * (l0 / l15) / 1000
  
  ltr.nmr <- nmr.rate * srb * nrr * tb.adj * (l0 / l15) / 1000
  ltr.nmr.lower <- nmr.rate.lower * srb * nrr * tb.adj * (l0 / l15) / 1000
  ltr.nmr.upper <- nmr.rate.upper * srb * nrr * tb.adj * (l0 / l15) / 1000
  
  ltr.loss <- ltr.stb + ltr.nmr
  prop.stb <- ltr.stb / (ltr.stb + ltr.nmr)
  
  country_results <- tibble(
    ISO = CNTRY,
    country = country,
    region = region,
    year = YR,
    l15 = l15,
    l0 = l0,
    tfr = tfr,
    tb.adj = tb.adj,
    stb.rate = stb.rate,
    stb.rate.lower = stb.rate.lower,
    stb.rate.upper = stb.rate.upper,
    ltr.stb = ltr.stb,
    ltr.stb.percent = ltr.stb * 100,
    ltr.stb.per.lower = ltr.stb.lower * 100,
    ltr.stb.per.upper = ltr.stb.upper * 100,
    nmr.rate = nmr.rate,
    nmr.rate.lower = nmr.rate.lower,
    nmr.rate.upper = nmr.rate.upper,
    ltr.nmr = ltr.nmr,
    ltr.nmr.percent = ltr.nmr * 100,
    ltr.nmr.per.lower = ltr.nmr.lower * 100,
    ltr.nmr.per.upper = ltr.nmr.upper * 100,
    ltr.loss = ltr.loss,
    ltr.loss.percent = ltr.loss * 100,
    prop.stb = prop.stb * 100
  )
  
  country_het <- tibble(
    ISO = CNTRY,
    country = country,
    region = region,
    year = YR,
    l15 = l15,
    l0 = l0,
    srb = srb,
    nrr = nrr,
    tb.adj = tb.adj,
    stb.rate = stb.rate,
    nmr.rate = nmr.rate,
    ltr.stb = ltr.stb,
    ltr.stb.lower = ltr.stb.lower,
    ltr.stb.upper = ltr.stb.upper,
    ltr.nmr = ltr.nmr,
    ltr.nmr.lower = ltr.nmr.lower,
    ltr.nmr.upper = ltr.nmr.upper
  )
  
  list(
    country_results = country_results,
    country_het = country_het
  )
}

rows <- map(seq_len(nrow(unicef_data)), function(i) {
  make_country_rows(unicef_data[i, ])
})

country_results <- bind_rows(map(rows, "country_results"))
country_het <- bind_rows(map(rows, "country_het"))

write_csv(country_results, here::here("gen", "lt-agg-country.csv"))
write_csv(country_het, here::here("gen", "lt-het-country.csv"))


## ---- Table S2: LTR country results ----

country_results_2023 <- subset(country_results, year == 2023)

fn_dynamic_round <- function(x) ifelse(abs(x) < 1, round(x, 2), round(x, 1))
fn_dynamic_fmt <- function(x) ifelse(abs(x) < 1, sprintf("%.2f", x), sprintf("%.1f", x))

gt_tblS2 <- country_results_2023 %>%
  filter(year == 2023) %>% 
  arrange(region, country) %>% 
  mutate(
    across(c(ltr.stb.percent, ltr.stb.per.lower, ltr.stb.per.upper,
             ltr.nmr.percent, ltr.nmr.per.lower, ltr.nmr.per.upper,
             ltr.loss.percent, prop.stb), fn_dynamic_round),
    
    ltr.stb.display = paste0(
      ltr.stb.percent, " (",
      ltr.stb.per.lower, "–",
      ltr.stb.per.upper, ")"
    ),
    
    ltr.nmr.display = paste0(
      ltr.nmr.percent, " (",
      ltr.nmr.per.lower, "–",
      ltr.nmr.per.upper, ")"
    )
  ) %>% 
  select(region, country, tfr, stb.rate, ltr.stb.display,
         nmr.rate, ltr.nmr.display, ltr.loss.percent, prop.stb) %>% 
  mutate(nmr.rate = fn_dynamic_fmt(nmr.rate), # convert to character before gt()
         tfr = fn_dynamic_fmt(tfr),
         ltr.loss.percent = fn_dynamic_fmt(ltr.loss.percent),
         prop.stb = fn_dynamic_fmt(prop.stb)) %>%
  gt(groupname_col = "region") %>%
  tab_header(title = "Global Estimates of Lifetime Stillbirths and Neonatal Deaths, 2023") %>% 
  cols_label(
    region = md("Region"), 
    country = md("Country"), 
    tfr = md("Total Fertility Rate"),
    stb.rate = md("SBR"),
    ltr.stb.display = md("LT-SB"), 
    nmr.rate = md("NMR"),
    ltr.nmr.display = md("LT-NM"), 
    ltr.loss.percent = md("LT-SN (%)"),
    prop.stb = md("SB share (%)")
  ) %>% 
  tab_options(row_group.as_column = TRUE, table.font.size = 14)

gtsave(gt_tblS2, here::here("gen/tables", "table-S2_country-lt-results2.docx"))
