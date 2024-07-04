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
  modelsummary,
  readxl,
  janitor,
  tidyverse
)

source(here("scripts/00_set_up.R"))

# Define data ------------------------------------------------------------------
# States to include
states <- c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR")

# Municipalites to include within those states
municipalities <- c("SAN QUINTIN", "PLAYAS DE ROSARITO", "TIJUANA", "ENSENADA",
                  "COMONDU", "MULEGE", "LA PAZ", "LOS CABOS")

# Relevant offices where landings are reported
offices <- c(
  "BAHIA ASUNCION", "BAHIA TORTUGAS",
  "CD. CONSTITUCION", "EL ROSARIO",
  "ENSENADA", "GUERRERO NEGRO",
  "ISLA CEDROS", "PTO. ADOLFO LOPEZ MATEOS",
  "PUNTA ABREOJOS", "SAN CARLOS",
  "SAN JUANICO", "SAN QUINTIN",
  "TIJUANA", "VILLA DE JESUS MARIA"
)

# Species to remove (freshwater or not relevant to SSF)
spp_remove <- c("BESUGO",
                "BAGRE",
                "LOBINA",
                "RUBIA Y VILLAJAIBA",
                "CARPA",
                "LANGOSTINO",
                "ANCHOVETA",
                "ESMEDREGAL",
                "OSTION",
                "PECES DE ORNATO",
                "BARRILETE",
                "PETO",
                "CALAMAR")

# Species from the list that are often exported (but not only)
export_spp <- c("CAMARON",
                "ATUN",
                "LANGOSTA",
                "ERIZO",
                "PEPINO DE MAR",
                "ABULON",
                "PULPO")

# Read data --------------------------------------------------------------------
# Mexico's consumer price index, relative to 2019 mexican pesos
cpi <- readRDS(here("data", "raw", "cpi_t_rates.rds"))

# Coop info
coop_numbers <- read_excel(path = here("data", "raw", "Cooperativas- UnidadesEconomicas2020.xlsx"),
                           sheet = "cooperativas") %>%
  clean_names() %>%
  filter(str_detect(tipo, "CAPTURA"),
         estado %in% states,
         municipio %in% municipalities,
         oficina %in% offices) %>%
  mutate(aquaculture = 1 * str_detect(tipo, "ACUACULTURA"),
         wildcaught = 1 * str_detect(tipo, "CAPTURA")) %>%
  select(eu_rnpa = rnpa,
         aquaculture,
         wildcaught,
         location = localidad,
         municipality = municipio,
         estate = estado,
         ofice = oficina,
         full_time = emp_planta_number_of_members,
         part_time = emp_eventual,
         n_boats = activos_menores,
         aquaculture_assets = activos_inst_acuicolas) %>%
  distinct()

# Read and assign types. If it's "fisica", then it's a fisher. If it's moral AND
# it's in the coop_numbers, then it's a cooperative. Otherwise it is an enterprise.
eu_names_and_types <- read_csv(here("data", "raw", "eu_names_and_types.csv")) %>%
  select(eu_rnpa, fiscal_type = tipo_persona) %>%
  distinct() %>%
  mutate(eu_rnpa = fix_rnpa(eu_rnpa, length = 10),
         fishing_entity = case_when(fiscal_type == "Fisica" ~ "Fisher",
                                    eu_rnpa %in% unique(coop_numbers$eu_rnpa) ~ "Cooperative",
                                    T ~ "Enterprise"))

# Landings data
landings_raw <- readRDS(file = file.path("/Users/juancarlosvillasenorderbez/GitHub/",
                                         "data_mex_fisheries",
                                         "data",
                                         "mex_landings",
                                         "clean",
                                         "mex_landings_2000_2022.rds")) %>%
  filter(state %in% c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR"),
         !acuaculture_production == "SÍ" | is.na(acuaculture_production),
         live_weight > 0,
         value > 0)

## PROCESSING ##################################################################

# counts to report filters
# Number  of SSF in Baja
landings_raw %>%
  filter(fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique() %>%
  length()

# EUs that report in offices along the pacific coastline
eus_reporting_in <- landings_raw %>%
  filter(office_name %in% offices,
         !main_species_group %in% spp_remove,
         fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique()

# SSF eus that report in other offices in Baja
eus_reporting_out <- landings_raw %>%
  filter(!office_name %in% offices,
         !main_species_group %in% spp_remove,
         fleet == "small_scale") %>%
  pull(eu_rnpa) %>%
  unique()

# Keep EUS that ONLY report in the offices identified (i.e. they don't report in the Gulf)
eus_only_in <- eus_reporting_in[!(eus_reporting_in %in% eus_reporting_out)]
length(eus_only_in)

# Find EUs with ten years and that fit the other criteria above
eus_with_10 <- landings_raw %>%
  filter(office_name %in% offices,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in
         ) %>%
  mutate(
    period = case_when(year %in% c(2014:2016) ~ "MHW",
                       year %in% c(2020:2022) ~ "C19",
                       T ~ "Baseline")) %>%
  group_by(eu_rnpa) %>%
  summarize(n = n_distinct(year),
            n_period = n_distinct(period),
            .groups = "drop") %>%
  filter(n >= 10,
         n_period == 3) %>%
  pull(eu_rnpa) %>%
  unique()

length(eus_with_10)

eus_with_5_baseline <- landings_raw %>%
  filter(office_name %in% offices,
        !main_species_group %in% spp_remove,
        eu_rnpa %in% eus_only_in,
        eu_rnpa %in% eus_with_10,
        year <= 2013) %>%
  group_by(eu_rnpa) %>%
  summarize(n_years = n_distinct(year),
            .groups = "drop") %>%
  filter(n_years >= 3) %>%
  pull(eu_rnpa) %>%
  unique()

length(eus_with_5_baseline)

# Build a panel of landings by species, year, and economic unit
yr_eu_spp <- landings_raw %>%
  # Some filters are redundant, but good for consistency and guard rails
  filter(office_name %in% offices,
         eu_rnpa %in% eus_reporting_in,
         !eu_rnpa %in% eus_reporting_out,
         !main_species_group %in% spp_remove,
         eu_rnpa %in% eus_only_in,
         eu_rnpa %in% eus_with_10,
         eu_rnpa %in% eus_with_5_baseline,
         !eu_rnpa %in% c(
           "0203008990", "0203013891", "0203014717", "0311001200", "0311001648", "0203009949", "0203126933",
           "0301000089", "0304001068", "0203007836", "0203008107", "0311000541", "0311001366", "0311001549",
           "0203014519", "0303000673", "0308000231", "0308000249", "0308000447", "0311001077", "0311001390",
           "0203001391", "0203010954", "0203011887", "0203127105", "0311001218", "0313000077"
         )
  ) %>%
  mutate(taxa = case_when(
    main_species_group == "ABULON" ~ "Haliotis",
    main_species_group == "ALGAS" ~ "Algae",
    main_species_group == "ALMEJA" ~ "Bivalvia",
    main_species_group == "ATUN" ~ "Thunnus",
    main_species_group == "BANDERA" ~ "Gerreidae",
    main_species_group == "BAQUETA" ~ "Serranidae",
    main_species_group == "BERRUGATA" ~ "Sciaenidae",
    main_species_group == "BONITO" ~ "Scombridae",
    main_species_group == "CABRILLA" ~ "Serranidae",
    main_species_group == "CAMARON" ~ "Shrimp",
    main_species_group == "CARACOL" ~ "Gastropoda",
    main_species_group == "CAZON" ~ "Mustelus",
    main_species_group == "CORVINA" ~ "Sciaenidae",
    main_species_group == "ERIZO" ~ "Echinoida",
    main_species_group == "GUACHINANGO" ~ "Lutjanidae",
    main_species_group == "JAIBA" ~ "Callinectes",
    main_species_group == "JUREL" ~ "Carangidae",
    main_species_group == "LANGOSTA" ~ "Palinuridae",
    main_species_group == "LEBRANCHA" ~ "Mugilidae",
    main_species_group == "LENGUADO" ~ "Pleuronectiformes",
    main_species_group == "LISA" ~ "Mugilidae",
    main_species_group == "MACARELA" ~ "Scombridae",
    main_species_group == "MERO" ~ "Serranidae",
    main_species_group == "MOJARRA" ~ "Gerreidae",
    main_species_group == "OTRAS" ~ "Others",
    main_species_group == "PAMPANO" ~ "Carangidae",
    main_species_group == "PARGO" ~ "Lutjanidae",
    main_species_group == "PEPINO DE MAR" ~ "Stichopodidae",
    main_species_group == "PIERNA" ~ "Malacanthidae",
    main_species_group == "PULPO" ~ "Octopodidae",
    main_species_group == "RAYA Y SIMILARES" ~ "Batoidea",
    main_species_group == "ROBALO" ~ "Centropomidae",
    main_species_group == "RONCO" ~ "Haemulidae",
    main_species_group == "SARDINA" ~ "Clupeidae",
    main_species_group == "SARGAZO" ~ "Algae",
    main_species_group == "SIERRA" ~ "Scombridae",
    main_species_group == "TIBURON" ~ "Sharks"
  ),
  market = ifelse(main_species_group %in% export_spp, "Export", "Local"),
  period = case_when(year %in% c(2015:2016) ~ "MHW",
                     year %in% c(2020:2022) ~ "C19",
                     T ~ "Baseline"),
  period = fct_relevel(period, c("Baseline", "MHW", "C19"))) %>%
  group_by(year, period, eu_rnpa, eu_name, market, taxa) %>%
  summarize(revenue = sum(value, na.rm = T),
            landings = sum(live_weight, na.rm = T),
            .groups = "drop") %>%
  left_join(cpi, by = "year") %>%
  mutate(revenue = revenue * rate) %>% # Mean value of 1 MEX to USD during 2019
  select(-rate)

length(unique(yr_eu_spp$eu_rnpa))

# Build annual eu level panel --------------------------------------------------
yr_eu <- yr_eu_spp %>%
  group_by(year, period, eu_rnpa, eu_name) %>%
  summarize(revenue = sum(revenue, na.rm = T),
            landings = sum(landings, na.rm = T),
            .groups = "drop") %>%
  group_by(eu_rnpa) %>%
  mutate(
    std_rev = (revenue - mean(revenue[period == "Baseline"], na.rm = T)) / sd(revenue[period == "Baseline"], na.rm = T),
    std_land = (landings - mean(landings[period == "Baseline"], na.rm = T)) / sd(landings[period == "Baseline"], na.rm = T)) %>%
  ungroup()

# Run two steps above without filters, get variable ones, remove, and re-run.
yr_eu %>% filter(abs(std_rev) >= 5 | abs(std_land) >= 5) %>% pull(eu_rnpa) %>% unique()

# EU charcteristics ------------------------------------------------------------
taxa_simpson <- yr_eu_spp %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa, taxa) %>%
  summarize(revenue = sum(revenue),
            .groups = "drop") %>%
  group_by(eu_rnpa) %>%
  summarize(taxa_simpson = 1 - (sum((revenue / sum(revenue)) ^ 2)),
            .groups = "drop")

market_simpson <- yr_eu_spp %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa, market) %>%
  summarize(revenue = sum(revenue),
            .groups = "drop") %>%
  group_by(eu_rnpa) %>%
  summarize(mkt_simpson = 1 - (sum((revenue / sum(revenue)) ^ 2)),
            .groups = "drop")

pct_export <- yr_eu_spp %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa, market) %>%
  summarize(revenue = sum(revenue),
            .groups = "drop") %>%
  pivot_wider(names_from = market,
              values_from = revenue,
              values_fill = 0) %>%
  mutate(pct_export = (Export / (Export + Local))) %>%
  select(eu_rnpa, pct_export)

richness <- yr_eu_spp %>%
  filter(year <= 2013) %>%
  group_by(eu_rnpa) %>%
  summarize(n_spp = n_distinct(taxa),
            .groups = "drop")

cv <- yr_eu %>%
  group_by(eu_rnpa) %>%
  summarize(
    mean_rev = mean(revenue ,na.rm = T),
    mean_land = mean(landings, na.rm = T),
    cv_rev = sd(revenue, na.rm = T) / mean(revenue, na.rm = T),
    cv_land = sd(landings, na.rm = T) / mean(landings, na.rm = T),
    .groups = "drop"
  )

characteristics <- cv %>%
  left_join(taxa_simpson, by = "eu_rnpa") %>%
  left_join(market_simpson, by = "eu_rnpa") %>%
  left_join(pct_export, by = "eu_rnpa") %>%
  left_join(richness, by = "eu_rnpa") %>%
  left_join(coop_numbers, by = "eu_rnpa") %>%
  left_join(eu_names_and_types, by = "eu_rnpa")



# Summary tables
datasummary((`Species Group` = taxa) * ((`Revenue (USD[2019])` = revenue) + (`Landings (Kg)` = landings)) ~ mean + median + sd + max + min ,
            data = yr_eu_spp)

datasummary((`Market` = market) * ((`Revenue (USD[2019])` = revenue) + (`Landings (Kg)` = landings)) ~ mean + median + sd + max + min ,
            data = yr_eu_spp %>% group_by(year, eu_rnpa, market) %>%
              summarize(landings = sum(landings, na.rm = T), revenue = sum(revenue, na.rm = T)),
            .groups = "drop")

datasummary((`Revenue (USD[2019])` = revenue) + (`Landings (Kg)` = landings) ~ mean + sd + median + max + min,
            data = yr_eu)




## EXPORT ######################################################################
saveRDS(object = yr_eu_spp,
        file = here("data", "processed", "year_eu_spp.rds"))
saveRDS(object = yr_eu,
        file = here("data", "processed", "year_eu.rds"))
saveRDS(object = characteristics,
        file = here("data", "processed", "characteristics.rds"))





