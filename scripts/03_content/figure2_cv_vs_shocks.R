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
yr_eu <- readRDS(here("data", "processed", "year_eu.rds"))
characteristics <- readRDS(file = here("data", "processed", "characteristics.rds"))
period_difs <- readRDS(here("data", "output", "period_diffs.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
model <- fixest::feols(std_rev ~ 0 + period:eu_rnpa,
                       panel.id = ~eu_rnpa + year,
                       vcov = "NW",
                       data = yr_eu)

shock <- model %>%
  broom::tidy() %>%
  mutate(eu_rnpa = str_extract(term, "[:digit:]{10}"),
         term = str_remove_all(term, "period|:eu_rnpa|[:digit:]")) %>%
  select(eu_rnpa, term, estimate, std.error) %>%
  pivot_wider(names_from = term,
              values_from = c(estimate, std.error)) %>%
  left_join(characteristics, by = "eu_rnpa") %>%
  mutate(c19_sign = ifelse(estimate_C > 0, "C19 +", "C19 -"),
         mhw_sign = ifelse(estimate_MHW > 0, "MHW +", "MHW -"),
         group = paste(c19_sign, mhw_sign)) %>%
  select(eu_rnpa, n_spp, simpson, inv_simp,
         mean_rev,
         mean_land, cv_rev, cv_land,
         c19_shock = estimate_C,
         c19_se = std.error_C,
         c19_sign,
         mhw_shock = estimate_MHW,
         mhw_se = std.error_MHW,
         mhw_sign,
         group)

shock %>%
  count(group) %>%
  mutate(n = (n / sum(n)) * 100)

## VISUALIZE ###################################################################
p1 <- shock %>%
  ggplot(mapping = aes(x = c19_shock, y = mhw_shock)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = period_difs$estimate[2],
             color = period_palette["C19"],
             linetype = "dotted") +
  geom_hline(yintercept = period_difs$estimate[1],
             color = period_palette["MHW"],
             linetype = "dotted") +
  geom_density2d(color = "black",
                 bins = 5) +
  geom_point(aes(fill = inv_simp),
             size = 1.5) +
  scale_fill_binned(type = "viridis") +
  labs(x = "C19 shock",
       y = "MHW shock",
       fill = "1-Simpson") +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0), legend.direction = "horizontal") +
  guides(fill = guide_colorsteps(title.position = "top",
                                 barwidth = 5,
                                 barheight = 1,
                                 ticks = T,
                                 show.limits = T,
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
  coord_equal() +
  annotate(geom = "text",
           x = c(-3, 3, 1, -3),
           y = c(2, 2, -2, -2),
           label = c("21.1%", "15.8%", "21.8%", "41.4%"))


p3 <- shock %>%
  mutate(bin = case_when(between(inv_simp, 0, 1/3) ~ "Low",
                         between(inv_simp, 1/3, 2/3) ~ "Med",
                         between(inv_simp, 2/3, 1) ~ "High"),
         bin = fct_reorder(bin, inv_simp)) %>%
  ggplot(mapping = aes(x = c19_shock, y = mhw_shock)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", color = "red") +
  geom_point(aes(color = inv_simp),
             size = 0.5,
             shape = 16) +
  geom_density2d(color = "black",
                 bins = 5,
                 linewidth = 0.1) +
  scale_fill_binned(type = "viridis") +
  scale_color_binned(type = "viridis") +
  theme(legend.position = "None",
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "S")) +
  facet_wrap(~bin, ncol = 1) +
  coord_equal()

p13 <- plot_grid(p1, p3,
                 ncol = 2,
                 align = "h",
                 rel_widths = c(3.5, 1),
                 axis = "b")

startR::lazy_ggsave(plot = p13,
                    filename = "figure2_cv_vs_shocks.png",
                    width = 15,
                    heigh = 9)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
