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
  readxl,
  tidyverse
)

# Load data --------------------------------------------------------------------

nemer <- read_xlsx(path = here("data/raw/Histograma de metacategorías.xlsx"),
                   sheet = 2) %>%
  mutate(Metacategoria = str_replace(Metacategoria, pattern = "/", replacement = "/\n"),
         Metacategoria = fct_reorder(Metacategoria, N, max),
         Metacategoria = fct_relevel(Metacategoria, "Other"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
p <- ggplot(data = nemer,
       aes(x = Metacategoria,
           y = N,
           fill = Metacategoria)) +
  geom_col(color = "black",
           linewidth = 0.5) +
  theme_minimal(base_size = 7) +
  theme(legend.position = "None",
        axis.title.y = element_blank()) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~Cooperativa, ncol = 5) +
  labs(y = "Count")

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p,
                    filename = "figure4_metacategories",
                    width = 15,
                    height = 5)
