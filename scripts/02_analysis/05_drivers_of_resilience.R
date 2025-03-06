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
  fixest,
  modelsummary,
  magrittr,
  tidyverse
)

source(here("scripts/00_set_up.R"))

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "output", "shock_estimates.rds")) %>%
  mutate(fishing_entity = fct_relevel(fishing_entity, "Fisher", "Enterprise", "Cooperative"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

models <- feols(c(-1*MHW, -1*C19, cv_land) ~  taxa_simpson + pct_export | fishing_entity,
                data = shock)
models %>%
  set_names(nm = c("MHW", "C19", "CV")) %>%
  modelsummary(coef_map = c("taxa_simpson" = "Catch diversity",
                            "pct_export" = "\\% Export",
                            "fishing_entityEnterprise" = "Company",
                            "fishing_entityCooperative" = "Cooperative",
                            "fishing_entityFisher" = "Fisher"),
               gof_omit = c("IC|Adj|Std|FE|MSE"),
               stars = panelsummary:::econ_stars(),
               output = here("results", "tab", "tab2_drivers.tex"),
               title = "\\label{tab:drivers}Table 2 - Regression results for drivers of shock and stability in the face of Marine heatwaves (MHW) and COVID-19 disruptions for 245 economic units in the Baja California Peninsula.
               Column 1 shows results for the effect of catch diversity on revenue losses during the MHW,
               column 2 shows the effect of reliance on export markets on COVID-19 disruptions.
               Column 3 shows results for the effect of both drivers on stability the coefficient of variation.
               All specifications include fixed-effects by type of economic unit.",
               escape = F)


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
                    filename = "figure5_diversity_vs_cv",
                    width = 12 ,
                    height = 12)
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
