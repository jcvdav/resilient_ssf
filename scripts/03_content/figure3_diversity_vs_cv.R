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


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
taxa_vs_cv <- shock %>%
  ggplot(mapping = aes(x = taxa_simpson, y = cv_rev)) +
  geom_smooth() +
  geom_point(size = 2) +
  labs(x = "Taxonomic diversity (1 - Simpson)",
       y = "CV")

mkt_vs_cv <- shock %>%
  ggplot(mapping = aes(x = pct_export, y = cv_rev)) +
  geom_smooth() +
  geom_point(size = 2) +
  labs(x = "% Revenue from export markets",
       y = "CV")

p2 <- plot_grid(taxa_vs_cv,
                mkt_vs_cv,
                ncol = 1)


startR::lazy_ggsave(plot = p2,
                    filename = "figure3_diversity_vs_cv.png",
                    width = 9,
                    height = 18)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
