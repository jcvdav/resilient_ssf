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
shock <- readRDS(file = here("data", "processed", "cv_and_shocks.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
cv_vs_mhw <- shock %>%
  ggplot(mapping = aes(x = cv_revenue, y = mhw_shock)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(aes(fill = n_spp, size = cv_revenue), shape = 21) +
  scale_fill_gradientn(colours =  colorRamps::matlab.like2(n = 10)) +
  labs(x = "CV of Revenue",
       y = "MHW Z-score") +
  theme(legend.position = "None")

cv_vs_c19 <- shock %>%
  ggplot(mapping = aes(x = cv_revenue, y = c19_shock)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_point(aes(fill = n_spp, size = cv_revenue), shape = 21) +
  scale_fill_gradientn(colours =  colorRamps::matlab.like2(n = 10)) +
  labs(x = "CV of Revenue",
       y = "C19 Z-score",
       size = "CV Revenue") +
  theme(legend.position = "None")

p1 <- shock %>%
  ggplot(mapping = aes(x = mhw_shock, y = c19_shock)) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_smooth(method = "lm", color = "red") +
  geom_density2d(color = "black") +
  geom_point(aes(fill = n_spp),
             size = 2) +
  scale_fill_binned(type = "viridis") +
  labs(x = "MHW Z-score",
       y = "C19 Z-score") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.box = "horizontal") +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "# Spp"))

p2 <- shock %>%
  mutate(c19_sign = ifelse(c19_shock > 0, "C19 +", "C19 -"),
         mhw_sign = ifelse(mhw_shock > 0, "MHW +", "MHW -"),
         group = paste(c19_sign, mhw_sign)) %>%
  count(group) %>% mutate(n = n / sum(n)) %>%
  ggplot(aes(x = group, y = n)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(c(0, 0.01), 0)) +
  coord_flip()

p <- plot_grid(p1,
               p2,
               ncol = 1,
               labels = c("A", "B"),
               rel_heights = c(2, 1))

startR::lazy_ggsave(plot = p,
                    filename = "figure2_cv_vs_shocks.png",
                    width = 9,
                    height = 12)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
