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
  # modelr,
  panelsummary,
  fixest,
  modelsummary,
  magrittr,
  tidyverse
)

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "output", "shock_estimates.rds")) %>%
  mutate(fishing_entity = fct_relevel(fishing_entity, "Fisher", "Enterprise", "Cooperative"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

m1 <- feols(-1 * MHW ~ taxa_simpson,
            data = shock)
m2 <- feols(-1 * C19 ~ pct_export,
            data = shock)
m3 <- feols(cv_land ~ csw(taxa_simpson + pct_export + fishing_entity,  full_time + part_time + aquaculture + n_boats),
            data = shock)

panelsummary(list(m1, m2),
             list(m3[[1]], m3[[2]]),
             panel_labels = c("Panel A: Outcome variable is -1 * gamma^j_i",
                              "Panel B: Outcome variable is CV"),
             coef_map = c("taxa_simpson" = "Catch diversity",
                          "pct_export" = "% Export",
                          "full_time" = "Full time employees",
                          "part_time" = "Part time employees",
                          "aquaculture" = "Aquaculture",
                          "n_boats" = "# Boats",
                          "fishing_entityEnterprise" = "Company",
                          "fishing_entityCooperative" = "Cooperative",
                          "(Intercept)" = "Intercept"),
             gof_omit = c("IC|Adj|Std"),
             stars = "econ")
## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
taxa_vs_MHW <- shock %>%
  ggplot(mapping = aes(x = taxa_simpson, y = MHW)) +
  annotate(geom = "text",
           x = -0.175,
           y = -0.05,
           size = 2,
           angle = 90,
           label = "Less impacted") +
  annotate(geom = "text",
           x = -0.175,
           y = -0.3,
           size = 2,
           angle = 90,
           label = "More impacted") +
  geom_smooth(method = "lm")+
  coord_cartesian(xlim = c(0, 1),
                  clip = "off") +
  labs(x = "Catch diversity (D)",
       y = expression(hat(mu)[1]))

taxa_vs_cv <- shock %>%
  ggplot(mapping = aes(x = taxa_simpson, y = cv_land)) +
  annotate(geom = "text",
           x = -0.175,
           y = 0.875,
           size = 2,
           angle = 90,
           label = "Less stable") +
  annotate(geom = "text",
           x = -0.175,
           y = 0.55,
           size = 2,
           angle = 90,
           label = "More stable") +
  annotate(geom = "text",
           x = 0.05,
           y = 0.425,
           size = 2,
           label = "Less Diverse") +
  annotate(geom = "text",
           x = 0.95,
           y = 0.425,
           size = 2,
           label = "More Diverse") +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0.5, 0.95),
                  clip = "off") +
  geom_smooth(method = "lm")+
  labs(x = "Catch diversity (D)",
       y = "CV")

mkt_vs_C19 <- shock %>%
  ggplot(mapping = aes(x = pct_export, y = C19)) +
  geom_smooth(method = "lm") +
  coord_cartesian(clip = "off") +
  labs(x = "% Revenue from exports",
       y = expression(hat(mu)[2]))

mkt_vs_cv <- shock %>%
  ggplot(mapping = aes(x = pct_export, y = cv_land)) +
  geom_smooth(method = "lm") +
  annotate(geom = "text",
           x = 0.05,
           y = 0.425,
           size = 2,
           label = "Less reliant") +
  annotate(geom = "text",
           x = 0.95,
           y = 0.425,
           size = 2,
           label = "More reliant") +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0.5, 0.95),
                  clip = "off") +
  labs(x = "% Revenue from exports",
       y = "CV")

p2 <- plot_grid(
  taxa_vs_MHW,
  mkt_vs_C19,
  taxa_vs_cv,
  mkt_vs_cv,
  ncol = 2,
  labels = c("AUTO"))

p2


startR::lazy_ggsave(plot = p2,
                    filename = "figure3_diversity_vs_cv.png",
                    width = 12 ,
                    height = 12)
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
