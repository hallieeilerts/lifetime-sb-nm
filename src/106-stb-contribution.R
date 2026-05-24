###################################################################
#----------- Lifetime stillbirths and neonatal deaths ------------# 
###################################################################

## --- FILE 106-stb-contribution.R: assess contribution of stb to lt-sn by mortality transition stage ---

# load LTR data
ltr <- read_csv(here::here("gen", "lt-agg-country.csv"))
ltr_data <- subset(ltr, year == 2023)

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

## ---- Figure 3: relative contribution of LT-SB to LT-SN by mortality transition stage in highest and lowest burden countries ------

# set colour palette
my_palette <- viridis::plasma(n = 4, begin = 0.15, end = 0.85, direction = -1)[c(4,3,1)]

# set percentage for tails
p <- 0.10  
n_tail <- floor(nrow(ltr_data) * p)

# rank countries by ltr.loss
ltr_ranked <- ltr_data %>%
  arrange(ltr.loss) %>%
  mutate(rank = row_number())

# select bottom and top n_tail countries
ltr_tails <- bind_rows(
  ltr_ranked %>%
    slice_head(n = n_tail) %>%
    mutate(tail = "Bottom 10% lifetime stillbirth & neonatal mortality"),
  ltr_ranked %>%
    slice_tail(n = n_tail) %>%
    mutate(tail = "Top 10% lifetime stillbirth & neonatal mortality")
)

plot_tails <- ggplot(ltr_tails,
                aes(y = reorder(country, prop.stb),
                    x = prop.stb, fill = transition)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_vline(xintercept = 50, linetype = "dashed") +
  labs(x = "Contribution of stillbirth to LT-SN (%)", y = "",
       fill = "Stage in stillbirth and neonatal transition") +
  scale_fill_manual(values = my_palette) +
  facet_wrap(~tail, scales = "free_y", ncol = 1) +   
  theme_classic() +
  theme(axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        panel.grid = element_blank(),
        legend.position = "bottom", 
        strip.text = element_text(size = 16, face = "bold")
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.001, 0.001)))
plot_tails

# save plot
ggsave(here::here("gen/figures", "figure-3_tails.png"), plot = plot_tails, width = 10, height = 8, dpi = 400)

