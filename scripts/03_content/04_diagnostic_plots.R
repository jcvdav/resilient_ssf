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

main_model <- readRDS(file = here("data", "output", "mixed_effects_model.rds"))
free_intercept_model <- readRDS(file = here("data", "output", "mixed_effects_model_with_random_intercept.rds"))

## PROCESSING ##################################################################

# Wrap everything into a function ----------------------------------------------

diagnostic_plot <- function(model) {
  predicted <- yr_eu |>
    select(std_rev, period, MHW, C19, eu_rnpa) %>%
    mutate(predicted_std_rev = predict(model, newdata = .),
           residuals = std_rev - predicted_std_rev,
           period = fct_relevel(period, c("MHW", "C19", "Baseline")))

  # Predicted vs observed ------------------------------------------------------
  p1 <- ggplot(data = predicted,
               aes(x = predicted_std_rev, y = std_rev, fill = period)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 1) +
    scale_fill_brewer(palette = "Set1") +
    coord_equal() +
    labs(x = "Predicted",
         y = "Observed",
         fill = "Period")

  # Predicted vs oresidual -----------------------------------------------------
  p2 <- ggplot(data = predicted,
               aes(x = predicted_std_rev, y = residuals, fill = period)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 1) +
    scale_fill_brewer(palette = "Set1") +
    coord_equal() +
    labs(x = "Predicted",
         y = "Residuals",
         fill = "Period")

  # Observed vs residuals ------------------------------------------------------
  p3 <- ggplot(data = predicted,
               aes(x = std_rev, y = residuals, fill = period)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.5) +
    geom_point(alpha = 0.5, size = 1) +
    scale_fill_brewer(palette = "Set1") +
    coord_equal() +
    labs(x = "Observed",
         y = "Residuals",
         fill = "Period")

  # Histogram of residuals -----------------------------------------------------
  hist <- ggplot(data = predicted,
                 aes(x = residuals)) +
    geom_histogram(color = "black")

  # Combine into a single plot -------------------------------------------------
  p <- plot_grid(p1, p2, p3, hist,
                 labels = "AUTO")
}

## VISUALIZE ###################################################################
p <- diagnostic_plot(main_model)
p2 <- diagnostic_plot(free_intercept_model)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "s3")

startR::lazy_ggsave(plot = p,
                    filename = "s4")
