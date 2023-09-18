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

# Load data --------------------------------------------------------------------

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
period_difs <- fixest::feols(std_rev ~ period | eu_rnpa,
              panel.id = ~eu_rnpa + year,
              vcov = "NW",
              data = yr_eu) %>%
  broom::tidy() %>%
  mutate(term = str_remove(term, "period"),
         term = fct_relevel(term, c("MHW", "C19")))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

period_plot <- ggplot(data = period_difs,
                      aes(x = term, y = estimate, fill = term)) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  shape = 21) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = period_palette) +
  labs(x = "Period",
       y = "Estimate") +
  theme(legend.position = "None")

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
       fill = "Perod") +
  theme(legend.position = c(0.5, 1),
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal")

plot <- plot_grid(ts_plot, period_plot,
                  rel_widths = c(1, 0.25),
                  labels = "AUTO",
                  align = "hv")

plot


## EXPORT ######################################################################
startR::lazy_ggsave(
  plot = plot,
  filename = "ts_plot",
  width = 18,
  height = 6
)
