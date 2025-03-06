################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  ggExtra,
  cowplot,
  tidyverse
)

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
coefs <- readRDS(file = here("data", "output", "coefs.rds"))
# period_diffs <- readRDS(file = here("data", "output", "period_diffs.rds"))
characteristics <- readRDS(file = here("data", "processed", "characteristics.rds"))
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

shock <- coefs %>%
  left_join(characteristics, by = "eu_rnpa") %>%
  mutate(mhw_sign = ifelse(MHW > 0, "MHW +", "MHW -"),
         c19_sign = ifelse(C19 > 0, "C19 +", "C19 -"),
         group = paste(c19_sign, mhw_sign)) %>%
  mutate(taxa_bin = case_when(between(taxa_simpson, 0, 1/3) ~ "Low",
                              between(taxa_simpson, 1/3, 2/3) ~ "Medium",
                              between(taxa_simpson, 2/3, 1) ~ "High"),
         taxa_bin = fct_reorder(taxa_bin, taxa_simpson),
         mkt_bin = case_when(between(pct_export, 0, 1/3) ~ "Low",
                             between(pct_export, 1/3, 2/3) ~ "Medium",
                             between(pct_export, 2/3, 1) ~ "High"),
         mkt_bin = fct_reorder(mkt_bin, pct_export))

points_pct <- shock %>%
  group_by(group) %>%
  summarize(n = n(),
            MHW = 2.2 * mean(MHW),
            C19 = 2.2 * mean(C19)) %>%
  mutate(pct = paste0(round((n / sum(n)) * 100, 1), "%"))

nobs <- length(unique(shock$eu_rnpa))
# Percent negatively impacted by MHW
shock |>
  count(mhw_sign) |>
  mutate(pct = (n / nobs) * 100)
# Percent negatively impacted by C19
shock |>
  count(c19_sign) |>
  mutate(pct = (n / nobs) * 100)

## VISUALIZE ###################################################################

base_plot <- ggplot(data = shock,
                    mapping = aes(x = MHW, y = C19,
                                  group = 1)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_fill_viridis_d() +
  scale_size_manual(values = c(0.5, 1, 1.5))

main_cor_test <- cor.test(shock$MHW, shock$C19, method = "kendall") %>%
  broom::tidy()

main_cor_test

p1 <- base_plot +
  geom_rect(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
            fill = "gray90",
            color = "transparent") +
  geom_rect(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
            fill = "gray90",
            color = "transparent") +
  geom_density2d(color = "black",
                 bins = 5,
                 linewidth = 0.1) +
  geom_point(aes(fill = taxa_bin,
                 size = mkt_bin)) +
  geom_text(data = points_pct,
            aes(x = MHW, y = C19, label = pct),
                inherit.aes = FALSE) +
  labs(x = expression("Economic unit's response to MHW shock ("~hat(gamma)[1]~")"),
       y = expression("Economic unit's response to C19 shock ("~hat(gamma)[2]~")"),
       fill = "Catch diversity",
       size = "% Export") +
  theme(legend.position = "left") +
  guides(fill = guide_legend(title.position = "top",
                             show.limits = T,
                             frame.colour = "black",
                             ticks.colour = "black"),
         size = guide_legend(title.position = "top",
                             override.aes = list(fill = "black",
                                                 shape = 21,
                                                 color = "transparent")))

p <- ggMarginal(p1, type = "densigram")

startR::lazy_ggsave(plot = p,
                    filename = "figure4_concordance_of_shocks",
                    width = 12,
                    height = 9)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(shock,
        file = here("data", "output", "shock_estimates.rds"))


