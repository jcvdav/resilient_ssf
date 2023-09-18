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

# Load data --------------------------------------------------------------------
# shock <- readRDS(file = here("data", "processed", "cv_and_shocks.rds"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
s_vs_cv <- shock %>%
  ggplot(mapping = aes(x = n_spp, y = cv_rev)) +
  geom_smooth(method = "lm") +
  geom_point(size = 2) +
  labs(x = "# Spp",
       y = "CV") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "# Spp"))

S_vs_cv <- shock %>%
  ggplot(mapping = aes(x = 1 - simpson, y = cv_rev)) +
  geom_smooth(method = "lm") +
  geom_point(size = 2) +
  labs(x = "1 - Simpson",
       y = "CV")

p2 <- plot_grid(s_vs_cv, S_vs_cv, ncol = 1)


startR::lazy_ggsave(plot = p2,
                    filename = "figure3_diversity_vs_cv.png",
                    width = 9,
                    height = 18)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
