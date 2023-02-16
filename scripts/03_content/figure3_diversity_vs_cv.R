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
  cowplot,
  colorRamps,
  tidyverse
)


update_geom_defaults(geom = "point",
                     new = list(shape = 21,
                                color = "black",
                                fill = "navyblue",
                                alpha = 0.8,
                                size = 3))

update_geom_defaults(geom = "smooth",
                     new = list(linetype = "dashed",
                                color = "black"))
theme_set(theme_bw())
theme_update(legend.background = element_blank())

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "processed", "cv_and_shocks.rds"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
s_vs_cv <- shock %>%
  ggplot(mapping = aes(x = n_spp, y = cv_revenue)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(size = 2) +
  labs(x = "# Spp",
       y = "CV") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "# Spp"))

S_vs_cv <- shock %>%
  ggplot(mapping = aes(x = 1 - simpson, y = cv_revenue)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(size = 2) +
  labs(x = "1 - Simpson",
       y = "CV")

p2 <- plot_grid(s_vs_cv, S_vs_cv)


ggsave(plot = p2,
       filename = here("results", "img", "figure3_diversity_vs_cv.png"),
       width = 6,
       height = 3)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
