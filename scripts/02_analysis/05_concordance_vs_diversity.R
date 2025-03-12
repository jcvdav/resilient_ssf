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
  tidyverse
)

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "output", "shock_estimates.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
taxa_correlation <- shock %>%
  group_by(taxa_bin) %>%
  nest() %>%
  mutate(cor_test = map(.x = data, ~broom::tidy(cor.test(.x$MHW, .x$C19, method = "kendall")))) %>%
  select(taxa_bin, cor_test) %>%
  unnest(cor_test) %>%
  mutate(string = paste0("T[b]==",round(estimate, 3),"~(p==", round(p.value, 3), ")"))

mkt_correlation <- shock %>%
  group_by(mkt_bin) %>%
  nest() %>%
  mutate(cor_test = map(.x = data, ~broom::tidy(cor.test(.x$MHW, .x$C19, method = "kendall")))) %>%
  select(mkt_bin, cor_test) %>%
  unnest(cor_test) %>%
  mutate(string = paste0("T[b]==",round(estimate, 3),"~(p==", round(p.value, 3), ")"))

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
base_plot <- ggplot(data = shock,
                    mapping = aes(x = MHW, y = C19,
                                  group = 1)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_fill_viridis_d() +
  scale_size_manual(values = c(0.5, 1, 1.5)) +
  labs(x = expression("Economic unit's impact during MHW shock"),
       y = expression("Economic unit's impact during C19 shock"),
       fill = "Catch diversity",
       size = "% Export")

p2 <- base_plot +
  geom_density2d(color = "black",
                 linewidth = 0.1,
                 bins = 5) +
  geom_point(aes(fill = taxa_bin,
                 size = mkt_bin)) +
  theme(legend.position = "None",
        panel.spacing = unit(0, "mm")) +
  facet_wrap(~taxa_bin, ncol = 1, as.table = F)

p3 <- base_plot +
  geom_density2d(color = "black",
                 linewidth = 0.1,
                 bins = 5) +
  geom_point(aes(fill = taxa_bin,
                 size = mkt_bin)) +
  scale_color_binned(type = "viridis") +
  theme(legend.position = "None",
        panel.spacing = unit(0, "mm")) +
  facet_wrap(~mkt_bin, ncol = 1,
             as.table = F)

legend <- get_plot_component(plot = p2 +
                               theme(legend.position = "bottom",
                                     legend.title.position = "top"),
                             pattern = "guide-box-bottom")

p <- plot_grid(
  p2, p3,
  ncol = 2,
  align = "h",
  axis = "b",
  labels = "AUTO")

pp <- plot_grid(p, legend, ncol = 1, rel_heights = c(1, 0.2))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = pp,
                    filename = "figure5_concordance_vs_diversity",
                    width = 11,
                    height = 10)
