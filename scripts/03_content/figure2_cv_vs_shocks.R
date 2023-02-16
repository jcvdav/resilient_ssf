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

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
cv_vs_mhw <- shock %>%
  ggplot(mapping = aes(x = cv_revenue, y = mhw_shock_r)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(aes(fill = n_spp)) +
  scale_fill_gradientn(colours =  colorRamps::matlab.like2(n = 10)) +
  labs(x = "CV of Revenue",
       y = "MHW Z-score") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "# Spp"))

cv_vs_c19 <- shock %>%
  ggplot(mapping = aes(x = cv_revenue, y = c19_shock_r)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(aes(fill = n_spp)) +
  scale_fill_gradientn(colours =  colorRamps::matlab.like2(n = 10)) +
  labs(x = "CV of Revenue",
       y = "C19 Z-score") +
  theme(legend.position = "None")

mhw_vs_c19 <- shock %>%
  ggplot(mapping = aes(x = mhw_shock_r, y = c19_shock_r)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(aes(fill = n_spp)) +
  scale_fill_gradientn(colours =  colorRamps::matlab.like2(n = 10)) +
  labs(x = "MHW Z-score",
       y = "C19 Z-score") +
  theme(legend.position = "None")

p1 <- plot_grid(cv_vs_mhw,
                cv_vs_c19,
                mhw_vs_c19,
                ncol = 3)

ggsave(plot = p1,
       filename = here("results", "img", "figure2_cv_vs_shocks.png"),
       width = 9,
       height = 3)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
