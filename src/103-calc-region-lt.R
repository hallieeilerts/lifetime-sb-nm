###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 103-calc-region-lt.R: calculate LT-SB and LT-NM by SDG region ---

## ---- load UNICEF data ----

# UNICEF Excel sheets have metadata rows then an SDG section. We find
# that section, skip to it, filter for Lower/Median/Upper bounds, and
# reshape to long format.
read_unicef_regional <- function(filepath, sheet_name) {
  raw <- read_excel(filepath, sheet = sheet_name, col_names = FALSE)
  sdg_row <- which(str_detect(raw[[1]], "Sustainable Development Goal"))[1]

  df <- read_excel(filepath, sheet = sheet_name, skip = sdg_row) %>%
    rename(region = 1, bound = 2)

  # Only keep rows before the next section (blank row or new header)
  end <- which(is.na(df$region) | str_detect(df$region, "Estimates of") %in% TRUE)[1]
  if (!is.na(end)) df <- slice(df, 1:(end - 1))

  df %>%
    filter(bound %in% c("Lower", "Median", "Upper")) %>%
    select(region, bound, matches("^\\d{4}")) %>%
    mutate(across(matches("^\\d{4}"), as.numeric)) %>%
    pivot_longer(-c(region, bound), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(as.numeric(year))) %>%
    filter(year %in% 2000:2023) %>%
    drop_na(value)
}

sbr_df <- read_unicef_regional(
  here::here("data/Stillbirth-rate-and-deaths_2024.xlsx"),
  "SBR Regional estimates"
)

nmr_df <- read_unicef_regional(
  here::here("data/Neonatal_Mortality_Rates_2024.xlsx"),
  "NMR Regional estimates"
)

## ---- load WPP data ----

# NRR = net reproduction ratio (expected surviving daughters per woman)
# SRB = sex ratio at birth (male births per 100 female births)
wpp_demo <-
  read_csv(
    here::here("data/WPP2024_Demographic_Indicators_Medium.csv"),
    show_col_types = FALSE
  ) %>%
  filter(Time %in% 2000:2023) %>%
  select(wpp_region = Location, year = Time, NRR, SRB) %>%
  mutate(across(c(NRR, SRB), as.numeric)) %>%
  distinct(wpp_region, year, .keep_all = TRUE)

# l_0 = life table radix (100,000)
# l_15 = female survivors to age 15 (lx at age group "15-19")
wpp_lt <-
  read_csv(here::here("data/wpp_region.csv"), show_col_types = FALSE) %>%
  mutate(Time = as.integer(Time), lx = as.numeric(lx)) %>%
  filter(Time %in% 2000:2023, AgeGrp %in% c("0", "15-19")) %>%
  mutate(age = if_else(AgeGrp == "0", "l_0", "l_15")) %>%
  select(wpp_region = Location, year = Time, age, lx) %>%
  distinct(wpp_region, year, age, .keep_all = TRUE) %>%
  pivot_wider(names_from = age, values_from = lx)

wpp <- inner_join(wpp_demo, wpp_lt, by = c("wpp_region", "year"))

tfr_df <- read_csv(
  here::here("data/WPP2024_Demographic_Indicators_Medium.csv"),
  show_col_types = FALSE
) %>%
  filter(Time == 2023) %>%
  select(wpp_region = Location, TFR) %>%
  mutate(TFR = as.numeric(TFR)) %>%
  distinct(wpp_region, .keep_all = TRUE)

## ---- calculate lt indicators ----

# UNICEF and WPP use slightly different region names; map the five that
# differ so we can join on wpp_region.
region_mapping <- tribble(
  ~region, ~wpp_region,
  "Landlocked developing countries", "Land-locked Developing Countries (LLDC)",
  "Small island developing States", "Small Island Developing States (SIDS)",
  "Australia and New Zealand", "Australia/New Zealand",
  "Oceania (exc. Australia and New Zealand)", "Oceania (excluding Australia and New Zealand)",
  "Europe, Northern America, Australia and New Zealand", "Europe, Northern America, Australia, and New Zealand"
)

# Stack both outcomes, join WPP demographic components, and apply the
# Wilmoth (2009) formula. Both SBR and NMR are per 1,000 live births,
# so we divide by 1,000 to get per-live-birth rates.
sbr_lookup <- sbr_df %>%
  filter(str_to_lower(bound) == "median") %>%
  transmute(region, year, SBR = value) %>%
  distinct(region, year, .keep_all = TRUE)

ltr_results <-
  bind_rows(
    sbr_df %>% mutate(outcome = "stillbirth"),
    nmr_df %>% mutate(outcome = "neonatal_mortality")
  ) %>%
  left_join(region_mapping, by = "region") %>%
  mutate(wpp_region = coalesce(wpp_region, region)) %>%
  left_join(wpp, by = c("wpp_region", "year")) %>%
  left_join(sbr_lookup, by = c("region", "year")) %>%
  drop_na(NRR, SRB, l_0, l_15, SBR) %>%
  mutate(
    bound = str_to_lower(bound),
    ltr = (value / 1000) * NRR * (SRB / 100 + 1) * (1 / (1 - (SBR / 1000))) * l_0 / l_15
  ) %>%
  select(outcome, region, bound, year, ltr) %>%
  pivot_wider(names_from = bound, values_from = ltr, names_prefix = "ltr_") %>%
  select(outcome, region, year, ltr_median, ltr_lower, ltr_upper) %>%
  arrange(outcome, region, year)


# save outputs
ltr_results %>%
  filter(outcome == "stillbirth") %>%
  select(-outcome) %>%
  write_csv(here::here("gen", "lt_stillbirth_by_sdg_region.csv"))
ltr_results %>%
  filter(outcome == "neonatal_mortality") %>%
  select(-outcome) %>%
  write_csv(here::here("gen", "lt_neonatal_mortality_by_sdg_region.csv"))

## ---- Table 1: LT region results for 2023 ----

# set sdg regions
sdg_regions <- c(
  "Central and Southern Asia",
  "Eastern and South-Eastern Asia",
  "Europe and Northern America",
  "Latin America and the Caribbean",
  "Northern Africa and Western Asia",
  "Oceania",
  "Sub-Saharan Africa",
  "World"
)

table_df <- ltr_results %>%
  filter(outcome == "stillbirth" & year == 2023 & region %in% sdg_regions) %>%
  select(region, stb_median = ltr_median, stb_lower = ltr_lower, stb_upper = ltr_upper) %>%
  left_join(
    ltr_results %>%  filter(outcome == "neonatal_mortality"  & year == 2023 & region %in% sdg_regions) %>%
      select(region, nm_median = ltr_median, nm_lower = ltr_lower, nm_upper = ltr_upper),
    by = "region"
  ) %>%
  left_join(sbr_df %>% 
              filter(bound == "Median" & year == 2023) %>% 
              select(-c(bound, year)) %>%
              rename(sbr = value), by = "region") %>%
  left_join(nmr_df %>% filter(bound == "Median" & year == 2023) %>% 
              select(-c(bound, year)) %>%
              rename(nmr = value), by = "region") %>%
  # UNICEF and WPP region names match at this level
  left_join(tfr_df, by = c("region" = "wpp_region")) %>%
  mutate(
    ltr_combined = stb_median + nm_median,
    pct_stb = stb_median / ltr_combined
  )

# Format for display
format_pct <- function(median, lower, upper) {
  paste0(
    sprintf("%.1f", median * 100),
    " (",
    sprintf("%.1f", lower * 100),
    "-",
    sprintf("%.1f", upper * 100),
    ")"
  )
}

display_df <- table_df %>%
  mutate(
    TFR = sprintf("%.2f", TFR),
    SBR = sprintf("%.1f", sbr),
    `LTR-STB (%)` = format_pct(stb_median, stb_lower, stb_upper),
    NMR = sprintf("%.1f", nmr),
    `LTR-NM (%)` = format_pct(nm_median, nm_lower, nm_upper),
    `LTR-STB+NM (%)` = sprintf("%.1f", ltr_combined * 100),
    `STB share (%)` = sprintf("%.1f", pct_stb * 100)
  ) %>%
  select(
    Region = region, TFR, SBR, `LTR-STB (%)`,
    NMR, `LTR-NM (%)`, `LTR-STB+NM (%)`, `STB share (%)`
  ) %>%
  arrange(Region)


# create Word table
ft <- flextable(display_df) %>%
  set_caption(caption = "Lifetime risk of stillbirth and neonatal mortality by SDG region, 2023") %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  align(align = "right", j = 2:8, part = "all") %>%
  align(align = "left", j = 1, part = "all") %>%
  add_footer_lines(
    paste0(
      "SBR = stillbirth rate per 1,000 total births (UNICEF 2024). ",
      "NMR = neonatal mortality rate per 1,000 live births (UNICEF 2024). ",
      "TFR = total fertility rate (WPP 2024). ",
      "LT = lifetime total, per 100 girls; ",
      "95% uncertainty intervals in parentheses. ",
      "SB share = LT-SB as a percentage of the combined LT-SN."
    )
  ) %>%
  fontsize(size = 8, part = "footer")

doc <- read_docx() %>%
  body_add_par("Table 1", style = "heading 1") %>%
  body_add_flextable(ft)

output_path <- here::here("gen/tables", "table-1_sdg_region_2023.docx")
print(doc, target = output_path)
cat("Saved to:", output_path, "\n")

