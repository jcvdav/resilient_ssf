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

nemer <- read_xlsx(path = here("data/raw/Histograma_de_metacategorias.xlsx"),
                   sheet = 2) %>%
  mutate(Metacategoria = str_replace(Metacategoria, pattern = "/", replacement = "/\n"),
         Metacategoria = fct_reorder(Metacategoria, N, max),
         Metacategoria = fct_relevel(Metacategoria, "Other")) %>%
  group_by(Cooperativa) %>%
  mutate(n = sum(N)) %>%
  group_by(Metacategoria) |>
  mutate(n2 = sum(N)) |>
  ungroup() %>%
  mutate(Cooperativa = paste0(Cooperativa, "\n(N = ", n, ")"),
         Cooperativa = str_to_title(Cooperativa),
         Metacategoria = paste0(Metacategoria, "\n(N = ", n2, ")"),
         Metacategoria = fct_reorder(Metacategoria, n2))

isa <- read_xlsx(path = here("data/raw/Social Problems_AartJCV.xlsx"),
                 sheet = 1, range = "A2:F9") |>
  pivot_longer(cols = c(2:6),
               names_to = "coop_code",
               values_to = "value") |>
  # replace_na(list(value = 0)) |>
  rename(problem_code = Problem) |>
  mutate(coop_code = as.numeric(coop_code),
         problem_code = as.numeric(problem_code))

coops <- read_xlsx(path = here("data/raw/Social Problems_AartJCV.xlsx"),
                   sheet = 1, range = "I2:J7") |>
  rename(coop = Cooperative, coop_code = `#`) |>
  mutate(coop = str_to_title(coop))

problems <- read_xlsx(path = here("data/raw/Social Problems_AartJCV.xlsx"),
                      sheet = 1, range = "L2:M9") |>
  rename(problem = Problem, problem_code = `#`)

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
isa_combined <- isa |>
  left_join(coops, by = join_by("coop_code")) |>
  left_join(problems, by = join_by("problem_code")) |>
  select(coop, problem, value)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
p1 <- ggplot(data = nemer,
            aes(x = Metacategoria,
                y = N,
                fill = Metacategoria)) +
  geom_col(color = "black",
           linewidth = 0.5) +
  theme_minimal(base_size = 6) +
  theme(legend.position = "None",
        axis.title.y = element_blank()) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~Cooperativa, ncol = 5) +
  labs(y = "Count")

# Build table ------------------------------------------------------------------
isa_combined |>
  mutate(value = ifelse(!is.na(value), paste0(round(value * 100, 2), "%"), "-")) |>
  pivot_wider(names_from = coop, values_from = value) |>
  rename(Problem = problem) |>
  kableExtra::kbl(format = "latex",
                  label = "metacat",
                  booktabs = T,
                  caption = "Fraction of responses mentioning each type of problem (rows) by cooperative (columns). Responses come from n=250 individual workshop participants.") |>
  kableExtra::save_kable(file = here("results", "tab", "metacategories_table.tex"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
startR::lazy_ggsave(plot = p1,
                    filename = "figure3_metacategories",
                    width = 16,
                    height = 5.5)
