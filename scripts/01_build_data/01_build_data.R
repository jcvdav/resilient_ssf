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
  tidyverse
)

# Define data ------------------------------------------------------------------
oficinas <- c(
  "BAHIA ASUNCION", "BAHIA TORTUGAS",
  "CD. CONSTITUCION", "EL ROSARIO", "ENSENADA", "GUERRERO NEGRO",
  "ISLA CEDROS", "PTO. ADOLFO LOPEZ MATEOS", "PUNTA ABREOJOS", "SAN CARLOS",
  "SAN JUANICO", "SAN QUINTIN","TIJUANA",
  "VILLA DE JESUS MARIA"
)

spp <- c(
  "ABULON",
  "ALGAS",
  "ALMEJA",
  "ANCHOVETA",
  "ATUN",
  "BAGRE",
  "BANDERA",
  "BAQUETA",
  "BARRILETE",
  "BERRUGATA",
  "BESUGO",
  "BONITO",
  "CABRILLA",
  "CALAMAR",
  "CAMARON",
  "CARACOL",
  # "CARPA", #Fresh water
  "CAZON",
  # "CHARAL", #Fresh water
  # "CINTILLA", #Not distributed in the pacific
  "CORVINA",
  "ERIZO",
  # "ESMEDREGAL",
  "GUACHINANGO",
  "JAIBA",
  "JUREL",
  "LANGOSTA",
  # "LANGOSTINO",
  # "LEBRANCHA",
  "LENGUADO",
  "LISA",
  # "LOBINA",
  # "MACARELA",
  "MERO",
  "MOJARRA",
  # "OSTION",
  "OTRAS",
  "PAMPANO",
  "PARGO",
  # "PECES DE ORNATO",
  "PEPINO DE MAR",
  "PETO",
  "PIERNA",
  "PULPO",
  "RAYA Y SIMILARES",
  "ROBALO",
  "RONCO",
  # "RUBIA Y VILLAJAIBA",
  # "SARDINA",
  "SARGAZO",
  "SIERRA",
  "TIBURON"
  # "TRUCHA"
)


# Load data --------------------------------------------------------------------
# CPI
cpi <- readRDS(here("../ssf_shocks", "data", "processed", "cpi_t_rates.rds"))

# # X
# eu_rnpas <-
#   sf::st_read(dsn = here("../ssf_shocks", "data", "processed", "turf_polygons.gpkg")) %>%
#   sf::st_drop_geometry() %>%
#   pull(eu_rnpa) %>%
#   unique()

# Landings data
landings_raw <- readRDS(
  file = file.path(
    "/Users/juancarlosvillasenorderbez/GitHub/",
    "data_mex_fisheries",
    "data",
    "mex_landings",
    "clean",
    "mex_landings_2000_2022.rds"
  )
) %>%
  filter(state %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
         !acuaculture_production == "SÍ" | is.na(acuaculture_production))

# counts to report filters
# Number  of SSF in Baja
landings_raw %>%
  filter(fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique() %>%
  length()

spp_remove <- c("BESUGO", "BAGRE", "LOBINA", "RUBIA Y VILLAJAIBA", "CARPA", "LANGOSTINO", "ANCHOVETA", "ESMEDREGAL", "OSTION", "PECES DE ORNATO", "BARRILETE", "PETO")

# EUs that report in offices along the pacific coastline
eus_reporting_in <- landings_raw %>%
  filter(office_name %in% oficinas,
         !main_species_group %in% spp_remove,
         fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique()

# SSF eus that report in other offices in Baja
eus_reporting_out <- landings_raw %>%
  filter(!office_name %in% oficinas,
         !main_species_group %in% spp_remove,
         fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique()

# Keep EUS that ONLY report in the offices identified (i.e. they don't report in the Gulf)
eus_only_in <- eus_reporting_in[!(eus_reporting_in %in% eus_reporting_out)]
length(eus_only_in)

eus_with_10 <- landings_raw %>%
  filter(office_name %in% oficinas,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in,
         year >= 2005) %>%
  group_by(eu_rnpa) %>%
  summarize(n = n_distinct(year)) %>%
  ungroup() %>%
  filter(n >= 10) %>%
  pull(eu_rnpa) %>%
  unique()

length(eus_with_10)

# eus <- c( FROM OLD SHOCKS data
#   "0203000278", "0203000302", "0203000351", "0203002829", "0203004577",
#   "0203004726", "0203004866", "0203005673", "0203006168", "0203008149",
#   "0203008305", "0203008610", "0203008826", "0203008875", "0203008990",
#   "0203009261", "0203009527", "0203009949", "0203009956", "0203010715",
#   "0203011457", "0203011499", "0203012646", "0203014063", "0203126677",
#   "0203127311", "0301000089", "0301000097", "0301000105", "0301000113",
#   "0305000101", "0310000013", "0313000028", "0313000036"
# )

# Build a dataset of all SSF landiings for EUS operating in Baja's pacific coastline
baja_landings <- landings_raw %>%
  # Some filters are redundant, but good for consistency and guard rails
  filter(year >= 2005,
         office_name %in% oficinas,
         eu_rnpa %in% eus_reporting_in,
         !eu_rnpa %in% eus_reporting_out,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in,
         eu_rnpa %in% eus_with_10,
         !eu_rnpa %in% c( "0203014162", "0203014717", "0203008990", "0203015656", "0203013891", "0311001549")
  ) %>%
  mutate(case_when(main_species_group == "LEBRANCHA" ~ "LISA")) %>%
  group_by(year, eu_rnpa) %>%
  summarize(revenue = sum(value, na.rm = T),
            live_weight = sum(live_weight, na.rm = T),
            n_spp = n_distinct(main_species_group)) %>%
  ungroup() %>%
  mutate(
    period = case_when(year %in% c(2014:2016) ~ "Blob",
                       year %in% c(2020) ~ "Pandemic",
                       T ~ "Baseline"),
    period = fct_relevel(period, c("Baseline", "Blob", "Pandemic"))
  ) %>%
  group_by(eu_rnpa) %>%
  mutate(n_period = n_distinct(period)) %>%
  ungroup() %>%
  filter(n_period == 3) %>%
  # group_by(period) %>%
  # mutate(n_per_period = n_distinct()) %>%
  # ungroup() %>%
  # filter(!(period == "Pandemic" & n_per_period == 2)) %>%
  left_join(cpi, by = "year") %>%
  mutate(revenue = revenue * rate)

# coop_numbers <- read_excel("/Users/juancarlosvillasenorderbez/Downloads/CooperativePoints2022.xlsx") %>%
#   janitor::clean_names() %>%
#   drop_na(emp_planta)
## PROCESSING ##################################################################

# Filter for cooperatives we are focusing on -----------------------------------


ts <- baja_landings %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T),
    std_land = (live_weight - mean(live_weight[period == "Baseline"], na.rm = T)) / sd(live_weight[period == "Baseline"], na.rm = T)) %>%
  ungroup()

ts_plot <- ggplot(data = ts,
                  aes(x = year, y = std_rev)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(group = eu_rnpa),
            linewidth = 0.1,
            alpha = 0.1) +
  stat_summary(aes(fill = period),
               geom = "pointrange",
               fun.data = mean_sdl,
               na.rm = T,
               shape = 21,
               fun.args = list(mult = 1)) +
  scale_fill_manual(values = period_palette) +
  scale_y_continuous(limits = c(-3, 3),
                     labels = c(-3:3),
                     breaks = c(-3:3)) +
  labs(x = "Year",
       y = "Normalized revenues (Z-score)",
       fill = "Period")

startR::lazy_ggsave(
  plot = ts_plot,
  filename = "ts_plot",
  width = 18,
  height = 6
)

# Calculate coefficient of variation up to 2013 --------------------------------
cv_i <- baja_landings %>%
  filter(year <= 2014) %>%
  group_by(eu_rnpa) %>%
  summarize(mean_revenue = mean(revenue, na.rm = T),
            sd_revenue = sd(revenue, na.rm = T),
            mean_land = mean(live_weight, na.rm = T),
            sd_land = sd(live_weight, na.rm = T),
            n = n(),
            n_spp = max(n_spp)) %>%
  ungroup() %>%
  mutate(cv_revenue = sd_revenue / mean_revenue,
         cv_land = sd_land / mean_land) %>%
  select(-c(mean_revenue, sd_revenue))

# Calculate simpson's diversity index ------------------------------------------
simpson <- landings_raw %>%
  filter(year >= 2005,
         office_name %in% oficinas,
         eu_rnpa %in% eus_reporting_in,
         !eu_rnpa %in% eus_reporting_out,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in,
         eu_rnpa %in% eus_with_10,
         !eu_rnpa %in% c( "0203014162", "0203014717", "0203008990", "0203015656", "0203013891", "0311001549")
  ) %>%
  group_by(year, eu_rnpa, main_species_group) %>%
  summarize(revenue = sum(value)) %>%
  ungroup() %>%
  group_by(eu_rnpa, main_species_group) %>%
  summarize(revenue = mean(revenue)) %>%
  group_by(eu_rnpa) %>%
  summarize(simpson = sum((revenue / sum(revenue)) ^ 2))

coefs <- fixest::feols(std_rev ~ 0 + period:eu_rnpa,
                       panel.id = ~eu_rnpa + year,
                       vcov = "NW",
                       data = ts) %>%
  broom::tidy() %>%
  filter(str_detect(term, ":")) %>%
  mutate(eu_rnpa = str_extract(term, "[:digit:]{10}"),
         term = str_remove_all(term, "period|:eu_rnpa|[:digit:]")) %>%
  select(eu_rnpa, term, estimate, std.error) %>%
  pivot_wider(names_from = term,
              values_from = c(estimate, std.error)) %>%
  mutate(b = ifelse(estimate_Blob > 0, "Positive", "Negative"),
         p = ifelse(estimate_Pandemic > 0, "Positive", "Negative")) %>%
  left_join(cv_i, by = "eu_rnpa") %>%
  left_join(simpson, by = "eu_rnpa") %>%
  select(eu_rnpa, n, n_spp, cv_revenue, cv_land, simpson,
         c19_shock = estimate_Pandemic, c19_se = std.error_Pandemic, mhw_shock = estimate_Blob, mhw_se = std.error_Blob)

ggplot(coefs,
       aes(x = mhw_shock, y = c19_shock)) +
  geom_point(aes(fill = n_spp),
             shape = 21,
             color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)


## EXPORT ######################################################################
saveRDS(object = coefs,
        file = here("data", "processed", "cv_and_shocks.rds"))

