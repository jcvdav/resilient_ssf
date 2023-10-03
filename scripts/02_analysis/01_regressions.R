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
  modelr,
  magrittr,
  tidyverse
)

# Load data --------------------------------------------------------------------
shock <- readRDS(file = here("data", "output", "shock_estimates.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

m1 <- feols(-1 * MHW ~ taxa_simpson,
            data = shock)
m2 <- feols(-1 * C19 ~ pct_export,
            data = shock)
m3 <- feols(cv_land ~ csw(taxa_simpson + pct_export, aquaculture + n_boats + aquaculture_assets),
            data = shock)

list(m1, m2, m3[[1]], m3[[2]]) %>%
  set_names(c("MHW", "C19", "CV", "CV")) %>%
  modelsummary(coef_map = c("taxa_simpson" = "Catch diversity",
                            "pct_export" = "% Export",
                            "(Intercept)" = "Intercept"),
               gof_omit = c("IC|Std."),
               stars = panelsummary:::econ_stars())
## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
