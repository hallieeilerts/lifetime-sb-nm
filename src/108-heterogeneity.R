###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 108-heterogeneity.R: calculate country-level results and create figure

# set path
here::i_am("src/108-heterogeneity.R")

# load data
ltr <- read.csv(here::here("gen", "lt-het-country.csv"))
ltr2023 <- subset(ltr, year == 2023)


# Function: create population distributions -------------------------------

fn_createPopDist <- function(stepsize, n_groups){
  
  # Generate population distributions for n subgroups
  grids <- expand.grid(rep(list(seq(0, 1, by = stepsize)), n_groups))
  # Keep only combinations that sum to 1
  grids <- grids[rowSums(grids) == 1, ]
  # Keep only rows where there are no zeros (unless n_groups == 0)
  if(n_groups != 1){
    grids <- grids[apply(grids == 0, 1, sum) == 0,]
  }else{
    grids <- as.data.frame(grids)
  }
  names(grids) <- paste0("Group", 1:n_groups)
  # reshape long
  gridsWide <- grids %>%
    mutate(group_id = 1:n()) %>%
    pivot_longer(cols = -group_id) %>%
    mutate(n_groups = n_groups) %>%
    rename(dist = value) %>%
    select(n_groups, group_id, name, dist)
  
  return(gridsWide)
}

# Generate population distributions for different numbers of subgroups
dat1 <- fn_createPopDist(.1, 1)
dat2 <- fn_createPopDist(.1, 2)
dat3 <- fn_createPopDist(.1, 3)
dat4 <- fn_createPopDist(.1, 4)
dat5 <- fn_createPopDist(.1, 5)
dat <- rbind(dat1, dat2, dat3, dat4, dat5)

# Create id for every population
datid <- dat %>%
  select(n_groups, group_id) %>%
  distinct() %>%
  mutate(pop_id = 1:n())
dat <- datid %>%
  full_join(dat, by = join_by(n_groups, group_id)) %>%
  select(pop_id, n_groups, group_id, name, dist)
dat_popdist <- dat
# plot population distributions by n subgroups
p1 <- dat %>%
  ggplot() +
  geom_bar(aes(x=group_id, y = dist, fill = name), stat= "identity") +
  coord_flip() +
  facet_wrap(~n_groups, nrow = 1) +
  labs(x = "population id", x = "distribution", subtitle = "number of pop subgroups", 
       title = "composition of simulated populations") +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
p1
#ggsave(here::here("output", "simulation-popdist.png"), plot = p1, width = 8, height = 5, dpi = 400, bg = "white")


# Function: assign sbr and nrr to population subgroups --------------------

# create function to assign sbr and nrr to subgroups within a population
# and always add up to same overall sbr and nrr
fn_gen_ratecombos <- function(dist, 
                              overall_sbr, overall_nrr,
                              sbr_min, sbr_max,
                              nrr_min, nrr_max, 
                              rho_sbr, rho_nrr) {
  
  ## sbr_min, sbr_max, nrr_min, nrr_max
  # these arguments control the shape of the scaling vectors.
  # they are not the final numeric bounds of sbr or nrr in the subgroups.
  # the scaling step will always push the numbers away from those limits to match the overall sbr or nrr.
  
  ## rho_sbr, rho_nrr
  # these arguments control how subgroup rates differ across subgroups.
  
  # rho_sbr controls how unequal sbr is across subgroups
  # it is restricted to [0, 1], fixing the ordering from low to high SBR.
  #   rho_sbr = 0 : all groups have similar SBR (no inequality)
  #   rho_sbr = 1 : maximal spread from low to high SBR
  
  # rho_nrr controls how NRR varies relative to the SBR ordering.
  #   rho_nrr = 1  : NRR increases with SBR (positive association)
  #   rho_nrr = 0  : no systematic relationship
  #   rho_nrr = -1 : NRR decreases with SBR (negative association)
  
  # number of groups in population
  n <- length(dist)
  
  # SBR vectors for subgroup patterns
  # Subgroup ordering is fixed by SBR (low to high across groups)
  sbr_base <- seq(sbr_min, sbr_max, length.out = n)
  # reversed pattern (high to low), used only for interpolation
  sbr_rev  <- rev(sbr_base)
  sbr_unscaled <- rho_sbr * sbr_base + (1 - rho_sbr) * sbr_rev
  # scale to match overall sbr
  sbr <- sbr_unscaled * (overall_sbr / sum(dist * sbr_unscaled))
  
  # NRR vectors for subgroup patterns
  # increasing pattern across groups
  nrr_base <- seq(nrr_min, nrr_max, length.out = n)
  # decreasing pattern across groups
  nrr_rev <- rev(nrr_base)
  # transform rho
  alpha_nrr <- (rho_nrr + 1) / 2 
  nrr_unscaled <- alpha_nrr * nrr_base + (1 - alpha_nrr) * nrr_rev
  nrr <- nrr_unscaled * (overall_nrr / sum(dist * nrr_unscaled))
  
  return(data.frame(
    sbr = sbr,
    nrr = nrr
  ))
}

# restrict rho_sbr >= 0 to fix group ordering by SBR (low to high)
# and vary rho_nrr to control how NRR aligns with that ordering
# nrr can increase or decrease relative to SBR
rho_sbr <- seq(0, 1, by = 0.25)
rho_nrr <- seq(-1, 1, by = 0.25)
# expand grid for every possible combination of sbr and nrr patterns
rho_grid <- expand_grid(
  rho_sbr = rho_sbr,
  rho_nrr = rho_nrr
)
nrow(rho_grid) # 45

# expand subgroup pattern vectors by population distributions
# each population is combined with every (rho_sbr, rho_nrr) pattern
dat_expanded <- dat %>%
  tidyr::crossing(rho_grid) %>%
  arrange(pop_id, rho_sbr, rho_nrr, group_id)
nrow(dat_expanded) # 29790

# Identify test cases -----------------------------------------------------

# Categorize countries into SBR-NRR low-low, low-high, high-low, high-high
ltr2023 <- ltr2023 %>%
  mutate(
    sbr_cat = if_else(stb.rate <= median(stb.rate, na.rm = TRUE), "Low SBR", "High SBR"),
    nrr_cat = if_else(nrr <= median(nrr, na.rm = TRUE), "Low NRR", "High NRR"),
    rate_group = case_when(
      sbr_cat == "Low SBR"  & nrr_cat == "Low NRR"  ~ "Low-Low",
      sbr_cat == "Low SBR"  & nrr_cat == "High NRR" ~ "Low-High",
      sbr_cat == "High SBR" & nrr_cat == "Low NRR"  ~ "High-Low",
      sbr_cat == "High SBR" & nrr_cat == "High NRR" ~ "High-High"
    )
  )

# Identify centers of each category
df_centers <- ltr2023 %>%
  group_by(rate_group) %>%
  summarise(
    srb_c = mean(srb, na.rm = TRUE),
    nrr_c = mean(nrr, na.rm = TRUE),
    .groups = "drop"
  )

# Select countries closest to center of each group as test cases
df_test_cases <- ltr2023 %>%
  left_join(df_centers, by = "rate_group") %>%
  mutate(dist = (srb - srb_c)^2 + (nrr - nrr_c)^2) %>%
  group_by(rate_group) %>%
  slice_min(dist, n = 1) %>%
  select(ISO, country, region, year, nrr, stb.rate, l15, l0, srb, rate_group)


# Assign sbr and nrr to population subgroups ------------------------------

# For each test case, assign subgroup sbr and nrr for all numbers of subgroups with different population distributions and sorted/contrasting SBR and NRR subgroup values

all_res <- data.frame()
for(i in 1:nrow(df_test_cases)){
  
  mydat <- df_test_cases[i,]
  
  message("Processing ", mydat$rate_group)
  
  # set overall sbr and nrr values
  overall_sbr <- mydat$stb.rate
  sbr_min <- overall_sbr * 0.25
  sbr_max <- overall_sbr * 2
  overall_nrr <- mydat$nrr
  nrr_min <- overall_nrr * 0.25
  nrr_max <- overall_nrr * 1.75
  
  # calculate subgroup sbr and nrr's that would add up to same total for every possible
  # population distribution and directionality of sbr/nrr vectors
  dat_res <- dat_expanded %>%
    group_by(pop_id, rho_sbr, rho_nrr) %>%
    arrange(group_id, .by_group = TRUE) %>%
    group_modify(~ {
      
      res <- fn_gen_ratecombos(
        dist = .x$dist, # .x refers to data from the current group
        overall_sbr = overall_sbr,
        overall_nrr = overall_nrr,
        sbr_min = sbr_min,
        sbr_max = sbr_max,
        nrr_min = nrr_min,
        nrr_max = nrr_max,
        rho_sbr = unique(.y$rho_sbr), # .y refers to the grouping key for the current group
        rho_nrr = unique(.y$rho_nrr)
      )
      
      bind_cols(.x, res[, c("sbr", "nrr")])
    }) %>%
    ungroup()
  
  # Check that all weighted subgroup nrr/sbr add up to overall values
  sum_check <- dat_res %>%
    group_by(pop_id, rho_sbr, rho_nrr, n_groups) %>%
    summarise(
      sbr_total = sum(dist * sbr),
      nrr_total = sum(dist * nrr),
      .groups = "drop"
    ) %>%
    filter(round(sbr_total, 3) != round(overall_sbr, 3) |
             round(nrr_total, 1) != round(overall_nrr,1)) %>%
    nrow()
  if(sum_check == 0){
    message("Sums checked, all add up to overall sbr and nrr")
  }else{
    message("Error")
  }
  
  # Merge on identifying information
  dat_res <- dat_res %>%
    mutate(ISO = mydat$ISO,
           country = mydat$country,
           region = mydat$region,
           year = mydat$year,
           l0 = mydat$l0,
           l15 = mydat$l15,
           srb = mydat$srb,
           rate_group = mydat$rate_group) 
  
  all_res <- rbind(all_res, dat_res)
  
}

# Calculate LT indicators -----------------------------------------------------------

dat_ltr <- all_res %>%
  mutate(ltr_stb_subgrp = sbr * nrr * srb * (l0 / l15)) %>%
  group_by(ISO, country, region, year, rate_group, pop_id, rho_sbr, rho_nrr, n_groups) %>%
  summarise(ltr_stb = sum(dist * ltr_stb_subgrp), 
            pop_var = var(dist), # large variance = more concentrated in a few subgroups
            p = dist[dist > 0],
            H =  -sum(p * log(p)),
            pop_eveness = H /log(length(p)), # high eveness = equally spread across subgroups
            .groups = "drop")

# subset naive ltr, which is that which would be calculated for one subgroup
dat_ltr_naive <- dat_ltr %>%
  filter(n_groups == 1) %>%
  select(ISO, ltr_stb) %>% 
  mutate(ltr_stb = round(ltr_stb, 4)) %>%
  distinct() %>%
  rename(ltr_stb_naive = ltr_stb)
# merge on
dat_ltr <- dat_ltr %>%
  left_join(dat_ltr_naive)

# hline dat for naive ltr
hline_dat <- dat_ltr %>%
  filter(!is.na(ltr_stb_naive)) %>%
  select(country, rate_group, ltr_stb_naive) %>%
  distinct()

# IQR for sierra leone
dat_ltr  %>%
  filter(n_groups == 5 & country == "Sierra Leone") %>%
  summarise(
    Q1 = quantile(ltr_stb/10, 0.25),
    median = median(ltr_stb/10),
    Q3 = quantile(ltr_stb/10, 0.75),
    IQR = IQR(ltr_stb/10)
  )
hline_dat %>% 
  filter(country == "Sierra Leone") %>%
  mutate(ltr_stb_naive/10)


# Minimum and maximum bias -------------------------------------------------

# select pop_id with largest upward bias
ctry_max <- dat_ltr  %>%
  filter(n_groups == 5) %>%
  group_by(country, rate_group, n_groups) %>%
  mutate(ltr_stb_max = max(ltr_stb)) %>%
  filter(ltr_stb == ltr_stb_max) %>%
  select(rate_group, country, n_groups, pop_id, rho_sbr, rho_nrr) %>%
  distinct() %>%
  mutate(bias = "upward")
# select pop_id with largest downward bias
ctry_min <- dat_ltr  %>%
  filter(n_groups == 5) %>%
  group_by(country, rate_group, n_groups) %>%
  mutate(ltr_stb_min = min(ltr_stb)) %>%
  filter(ltr_stb == ltr_stb_min) %>% 
  select(rate_group, country, n_groups, pop_id, rho_sbr, rho_nrr) %>%
  distinct() %>%
  mutate(bias = "downward")
ctry_bias <- rbind(ctry_max, ctry_min)

# Combined plot -----------------------------------------------------------

p1 <- dat_popdist %>%
  filter(n_groups == 5) %>%
  mutate(name = case_when(
    name == "Group1" ~ "Subgroup 1",
    name == "Group2" ~ "Subgroup 2",
    name == "Group3" ~ "Subgroup 3",
    name == "Group4" ~ "Subgroup 4",
    name == "Group5" ~ "Subgroup 5",
    TRUE ~ NA
  )) %>%
  ggplot() +
  geom_bar(aes(x=group_id, y = dist, fill = name), stat= "identity") +
  scale_fill_viridis_d(option = "plasma") +
  coord_flip() +
  labs(x = "Simulation", y = "Pop. distribution", 
       #title = "Relative distribution of simulated population subgroups",
       title = "A: Relative distributions of simulated\npopulations",
       subtitle = "5 subgroups",
       fill = "") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none") 


cols <- viridisLite::plasma(5)[2:3]
p2 <- all_res %>%
  inner_join(ctry_bias, by = c("rate_group", "country", "n_groups","pop_id", "rho_sbr", "rho_nrr")) %>% 
  filter(country == "Sierra Leone") %>%
  mutate(bias = case_when(
    bias == "downward" ~ "Downward",
    bias == "upward" ~ "Upward",
    TRUE ~ NA
  )) %>%
  ggplot() +
  geom_point(aes(x = sbr, y = nrr, size = dist, color = bias)) +
  labs(title = "C: Simulated populations with largest\nupward and downward bias in LT-SB",
       size = "Pop. distribution", color = "Bias direction",
       x = "SBR", y = "NRR") +
  scale_color_manual(values = c("Upward" = cols[1],"Downward" = cols[2])) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

p3 <- dat_ltr  %>%
  filter(n_groups == 5 & country == "Sierra Leone") %>%
  ggplot() +
  geom_hline(data = hline_dat %>% filter(country == "Sierra Leone"), 
             aes(yintercept = ltr_stb_naive/10), color = "red") +
  geom_boxplot(aes(x=country, y = ltr_stb/10)) +
  labs(title = "B: Distribution of simulated LT-SB",
       subtitle = "Red line shows naive LT-SB",
       x = "", y = "LTR-SB") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

# save
emf("./gen/figures/figure-s3_het-sim.emf", width = 12, height = 5)
grid.arrange(p1, p3, p2, nrow = 1, widths = c(1, 1, 1))
dev.off()

