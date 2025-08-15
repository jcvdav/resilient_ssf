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

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
models %>%
  set_names(nm = c("MHW", "C19", "CV")) %>%
  modelsummary(coef_map = c("taxa_simpson" = "Taxonomic Diversity of Catch",
                            "pct_export" = "\\% Export",
                            "fishing_entityEnterprise" = "Company",
                            "fishing_entityCooperative" = "Cooperative",
                            "fishing_entityFisher" = "Fisher"),
               gof_omit = c("IC|Adj|Std|FE|MSE"),
               stars = panelsummary:::econ_stars(),
               output = here("results", "tab", "tab2_drivers.tex"),
               title = "\\label{tab:drivers}Table 2 - Regression results for drivers of shock and stability in the face of Marine heatwaves (MHW) and COVID-19 disruptions for 237 economic units in the Baja California Peninsula.
               Column 1 shows results for the effect of catch diversity on revenue losses during the MHW,
               column 2 shows the effect of reliance on export markets on COVID-19 disruptions.
               Column 3 shows results for the effect of both drivers on the coefficient of variation.
               All specifications include fixed-effects by type of economic unit.",
               escape = F)
