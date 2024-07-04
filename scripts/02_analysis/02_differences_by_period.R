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
  fixest,
  lme4,
  cowplot,
  tidyverse
)

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
yr_eu <- readRDS(here("data", "processed", "year_eu.rds"))
model <- readRDS(here("data", "output", "mixed_effects_model.rds"))

## PROCESSING ##################################################################
period_diffs <- tidy_lme4(model)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

period_plot <- ggplot(data = period_diffs,
                      mapping = aes(x = term, y = estimate, fill = term)) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  shape = 21,
                  size = 4,
                  fatten = 1,
                  linewidth = 2) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = period_palette) +
  labs(x = "Period",
       y = expression("Estimate ("~hat(mu)[1]~"or"~hat(mu)[2]~")")) +
  theme(legend.position = "None")

period_plot

# X ----------------------------------------------------------------------------
ts_plot <- ggplot(data = yr_eu,
                  aes(x = year, y = std_rev)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(group = eu_rnpa),
            linewidth = 0.1,
            alpha = 0.1) +
  stat_summary(aes(fill = period),
               geom = "pointrange",
               fun.data = mean_sdl,
               na.rm = T,
               shape = 21,
               fun.args = list(mult = 1)) +
  scale_fill_manual(values = period_palette) +
  scale_x_continuous(expand = c(0.01, 0)) +
  labs(x = "Year",
       y = "Normalized revenues (Z-score)",
       fill = "Period") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.5, 1),
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal",
        legend.title.position = "top")

plot <- plot_grid(ts_plot, period_plot,
                  rel_widths = c(1, 0.3),
                  labels = "AUTO",
                  align = "hv")

## EXPORT ######################################################################
startR::lazy_ggsave(
  plot = plot,
  filename = "fig2_ts_plot",
  width = 12,
  height = 6
)

saveRDS(object = period_diffs,
        file = here("data", "output", "period_diffs.rds"))

