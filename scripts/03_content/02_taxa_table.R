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
  kableExtra,
  tidyverse
)

# Load data --------------------------------------------------------------------

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

tab <- tribble(~"Reported", ~"Category assigned",
               "ABULON","Haliotis",
               "ALGAS","Algae",
               "ALMEJA","Bivalvia",
               "ATUN","Thunnus",
               "BANDERA","Gerreidae",
               "BAQUETA","Serranidae",
               "BERRUGATA","Sciaenidae",
               "BONITO","Scombridae",
               "CABRILLA","Serranidae",
               "CALAMAR","Squid",
               "CAMARON","Shrimp",
               "CARACOL","Gastropoda",
               "CAZON","Mustelus",
               "CORVINA","Sciaenidae",
               "ERIZO","Echinoida",
               "GUACHINANGO","Lutjanidae",
               "JAIBA","Callinected",
               "JUREL","Carangidae",
               "LANGOSTA","Palinuridae",
               "LEBRANCHA","Mugilidae",
               "LENGUADO","Pleuronectiformes",
               "LISA","Mugilidae",
               "MACARELA","Scombridae",
               "MERO","Serranidae",
               "MOJARRA","Gerreidae",
               "OTRAS","Others",
               "PAMPANO","Carangidae",
               "PARGO","Lutjanidae",
               "PEPINO DE MAR","Stichopodidae",
               "PIERNA","Malacanthidae",
               "PULPO","Octopodidae",
               "RAYA Y SIMILARES","Batoidea",
               "ROBALO","Centropomidae",
               "RONCO","Haemulidae",
               "SARDINA","Clupeidae",
               "SARGAZO","Algae",
               "SIERRA","Scombridae",
               "TIBURON","Sharks") %>%
  mutate(Reported = str_to_sentence(Reported)) %>%
  arrange(`Category assigned`)



kbl(tab,#format = "latex",
      linesep = "",
      booktabs = T) %>%
  kable_styling() %>%
  collapse_rows(columns = 2)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
