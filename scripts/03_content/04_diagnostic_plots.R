################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  lme4,
  tidyverse,
  cowplot
)

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
yr_eu <- readRDS(here("data/processed/year_eu.rds")) %>%
  mutate(baseline = 1 * (period == "Baseline"),
         MHW = 1 * (period == "MHW"),
         C19 = 1 * (period == "C19"))

model <- readRDS(file = here("data", "output", "mixed_effects_model.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
predicted <- yr_eu |>
  select(std_rev, period, MHW, C19, eu_rnpa) %>%
  mutate(predicted_std_rev = predict(model, newdata = .),
         residuals = std_rev - predicted_std_rev,
         period = fct_relevel(period, c("MHW", "C19", "Baseline")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = predicted,
       aes(x = predicted_std_rev, y = std_rev, fill = period)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Predicted",
       y = "Observed",
       fill = "Period")

p2 <- ggplot(data = predicted,
       aes(x = predicted_std_rev, y = residuals, fill = period)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Predicted",
       y = "Residuals",
       fill = "Period")

p3 <- ggplot(data = predicted,
       aes(x = std_rev, y = residuals, fill = period)) +
  geom_point(alpha = 0.5, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Observed",
       y = "Residuals",
       fill = "Period")

hist <- ggplot(data = predicted,
               aes(x = residuals)) +
  geom_histogram(color = "black")

p <- plot_grid(p1, p2, p3, hist,
               labels = "AUTO")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "s3")
