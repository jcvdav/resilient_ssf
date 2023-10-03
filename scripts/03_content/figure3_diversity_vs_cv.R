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
taxa_vs_MHW <- shock %>%
  ggplot(mapping = aes(x = taxa_simpson, y = -MHW)) +
  geom_smooth(method = "lm")+
  labs(x = "Taxonomic diversity (1 - Simpson)",
       y = expression(hat(mu)[1]))

taxa_vs_cv <- shock %>%
  ggplot(mapping = aes(x = taxa_simpson, y = cv_land)) +
  geom_smooth(method = "lm")+
  labs(x = "Taxonomic diversity (1 - Simpson)",
       y = "CV")

mkt_vs_C19 <- shock %>%
  ggplot(mapping = aes(x = pct_export, y = -C19)) +
  geom_smooth(method = "lm") +
  labs(x = "% Revenue from export markets",
       y = expression(hat(mu)[2]))

mkt_vs_cv <- shock %>%
  ggplot(mapping = aes(x = pct_export, y = cv_land)) +
  geom_smooth(method = "lm") +
  labs(x = "% Revenue from export markets",
       y = "CV")

p2 <- plot_grid(#taxa_vs_MHW,
                taxa_vs_cv,
                # mkt_vs_C19,
                mkt_vs_cv, ncol = 2)


startR::lazy_ggsave(plot = p2,
                    filename = "figure3_diversity_vs_cv.png",
                    width = 18,
                    height = 9)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
