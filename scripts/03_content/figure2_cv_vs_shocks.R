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
  mutate(bin = case_when(between(taxa_simpson, 0, 1/3) ~ "Low",
                         between(taxa_simpson, 1/3, 2/3) ~ "Med",
                         between(taxa_simpson, 2/3, 1) ~ "High"),
         bin = fct_reorder(bin, taxa_simpson))

shock %>%
  count(group) %>%
  mutate(n = (n / sum(n)) * 100)

## VISUALIZE ###################################################################
p1 <- shock %>%
  ggplot(mapping = aes(x = C19, y = MHW)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_density2d(color = "black",
                 bins = 5) +
  geom_point(aes(fill = taxa_simpson),
             size = 1.5) +
  scale_fill_binned(type = "viridis") +
  labs(x = expression("C19 shock ("~hat(beta)[2]~")"),
       y = expression("MHW shock ("~hat(beta)[1]~")"),
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
           x = c(-2, 2, 1, -2),
           y = c(1, 1, -1, -1),
           label = c("16.9%", "12.24%", "22.9%", "47.7%"))

spearman <- shock %>%
  group_by(bin) %>%
  nest() %>%
  mutate(cor_test = map(.x = data, ~broom::tidy(cor.test(.x$MHW, .x$C19, method = "spearman")))) %>%
  select(bin, cor_test) %>%
  unnest(cor_test) %>%
  mutate(string = paste0("rho==",round(estimate, 3),"~(p==", round(p.value, 3), ")"))

p3 <- shock %>%
  ggplot(mapping = aes(x = C19, y = MHW)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", color = "red") +
  geom_point(aes(color = taxa_simpson),
             size = 0.5,
             shape = 16) +
  geom_density2d(color = "black",
                 bins = 5,
                 linewidth = 0.1) +
  scale_fill_binned(type = "viridis") +
  scale_color_binned(type = "viridis") +
  theme(legend.position = "None",
        axis.title = element_blank(),
        panel.spacing = unit(0, "mm")) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title = "S")) +
  facet_wrap(~bin, ncol = 1) +
  geom_text(data = spearman,
            x = -0.8, y = 1.2,
            aes(label = string),
            size = 2,
            parse = T)

p13 <- plot_grid(p1, p3,
                 ncol = 2,
                 align = "h",
                 rel_widths = c(3, 1),
                 axis = "b",
                 labels = "AUTO")

startR::lazy_ggsave(plot = p13,
                    filename = "figure2_cv_vs_shocks.png",
                    width = 18,
                    heigh = 7.7)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(shock,
        file = here("data", "output", "shock_estimates.rds"))
