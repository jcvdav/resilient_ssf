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
model <- readRDS(here("data", "output", "mixed_effects_model.rds"))
characteristics <- readRDS(file = here("data", "processed", "characteristics.rds"))
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
period_diffs <- tidy_lme4(model)
coefs <- coef(model)$eu_rnpa %>%
  as_tibble(rownames = "eu_rnpa")


shock <- coefs %>%
  left_join(characteristics, by = "eu_rnpa") %>%
  mutate(mhw_sign = ifelse(MHW > 0, "MHW +", "MHW -"),
         c19_sign = ifelse(C19 > 0, "C19 +", "C19 -"),
         group = paste(c19_sign, mhw_sign)) %>%
  mutate(taxa_bin = case_when(between(taxa_simpson, 0, 1/3) ~ "Low",
                              between(taxa_simpson, 1/3, 2/3) ~ "Med",
                              between(taxa_simpson, 2/3, 1) ~ "High"),
         taxa_bin = fct_reorder(taxa_bin, taxa_simpson),
         mkt_bin = case_when(between(mkt_simpson, 0, 1/3) ~ "Low",
                             between(mkt_simpson, 1/3, 2/3) ~ "Med",
                             between(mkt_simpson, 2/3, 1) ~ "High"),
         mkt_bin = fct_reorder(mkt_bin, mkt_simpson))

shock %>%
  count(group) %>%
  mutate(n = (n / sum(n)) * 100)

## VISUALIZE ###################################################################

base_plot <- ggplot(data = shock,
                    mapping = aes(x = MHW, y = C19)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_fill_binned(type = "viridis")

cor.test(shock$MHW, shock$C19, method = "kendall") %>%
  broom::tidy()

p1 <- base_plot +
  geom_density2d(color = "black",
                 bins = 5) +
  geom_point(aes(fill = taxa_simpson),
             size = 1.5) +
  labs(x = expression("MHW shock ("~hat(beta)[1]~")"),
       y = expression("C19 shock ("~hat(beta)[2]~")"),
       fill = "1-Simpson") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal") +
  guides(fill = guide_colorsteps(title.position = "top",
                                 barwidth = 5,
                                 barheight = 1,
                                 ticks = T,
                                 show.limits = T,
                                 frame.colour = "black",
                                 ticks.colour = "black")) +
  annotate(geom = "text",
           x = c(-1, 1, 0.5, -1),
           y = c(2, 2, -2, -2),
           label = c("16.9%", "12.4%", "22.9%", "47.7%")) +
  annotate(geom = "text",
           x = 0.75,
           y = 2.9,
           size = 3,
           label = expression(tau==0.09~"("~p==0.019~")"))

correlation <- shock %>%
  group_by(taxa_bin) %>%
  nest() %>%
  mutate(cor_test = map(.x = data, ~broom::tidy(cor.test(.x$MHW, .x$C19, method = "kendall")))) %>%
  select(taxa_bin, cor_test) %>%
  unnest(cor_test) %>%
  mutate(string = paste0("tau==",round(estimate, 3),"~(p==", round(p.value, 3), ")"))

p3 <- base_plot +
  geom_density2d(color = "black",
                 linewidth = 0.1,
                 bins = 5) +
  geom_smooth(method = "lm", color = "red") +
  geom_point(aes(color = taxa_simpson),
             size = 0.5,
             shape = 16) +
  scale_color_binned(type = "viridis") +
  theme(legend.position = "None",
        axis.title = element_blank(),
        panel.spacing = unit(0, "mm")) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "S")) +
  facet_wrap(~taxa_bin, ncol = 1) +
  geom_text(data = correlation,
            x = 0, y = 2.5,
            aes(label = string),
            size = 2,
            parse = T)

p13 <- plot_grid(p1, p3,
                 ncol = 2,
                 align = "h",
                 rel_widths = c(2, 1),
                 axis = "b",
                 labels = "AUTO")

startR::lazy_ggsave(plot = p13,
                    filename = "figure2_cv_vs_shocks.png",
                    width = 12,
                    height = 12)


feols(C19 ~ csw(MHW:taxa_bin + pct_export, aquaculture + n_boats + aquaculture_assets),
      data = shock) %>%
  modelsummary::modelsummary(stars = panelsummary:::econ_stars())

feols(C19 ~ MHW + taxa_bin + mkt_bin,
      data = shock) %>%
  modelsummary::modelsummary(stars = panelsummary:::econ_stars())

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(shock,
        file = here("data", "output", "shock_estimates.rds"))


