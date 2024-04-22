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
  ggExtra,
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
  mutate(taxa_bin = case_when(between(taxa_simpson, 0, 1/3) ~ "Low",
                              between(taxa_simpson, 1/3, 2/3) ~ "Medium",
                              between(taxa_simpson, 2/3, 1) ~ "High"),
         taxa_bin = fct_reorder(taxa_bin, taxa_simpson),
         mkt_bin = case_when(between(pct_export, 0, 1/3) ~ "Low",
                             between(pct_export, 1/3, 2/3) ~ "Medium",
                             between(pct_export, 2/3, 1) ~ "High"),
         mkt_bin = fct_reorder(mkt_bin, pct_export))

points_pct <- shock %>%
  group_by(group) %>%
  summarize(n = n(),
            MHW = 2.5 * mean(MHW),
            C19 = 2.5 * mean(C19)) %>%
  mutate(pct = paste0(round((n / sum(n)) * 100, 1), "%"))

## VISUALIZE ###################################################################

base_plot <- ggplot(data = shock,
                    mapping = aes(x = MHW, y = C19,
                                  group = 1)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_fill_viridis_d() +
  scale_size_manual(values = c(0.5, 1, 1.5))

cor.test(shock$MHW, shock$C19, method = "kendall") %>%
  broom::tidy()

p1 <- base_plot +
  geom_rect(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
            fill = "gray90",
            color = "transparent") +
  geom_rect(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
            fill = "gray90",
            color = "transparent") +
  geom_density2d(color = "black",
                 bins = 5,
                 linewidth = 0.1) +
  geom_point(aes(fill = taxa_bin,
                 size = mkt_bin)) +
  labs(x = expression("MHW shock ("~hat(gamma)[1]~")"),
       y = expression("C19 shock ("~hat(gamma)[2]~")"),
       fill = "Catch diversity",
       size = "% Export") +
  guides(fill = guide_legend(title.position = "top",
                             show.limits = T,
                             frame.colour = "black",
                             ticks.colour = "black"),
         size = guide_legend(title.position = "top",
                             override.aes = list(fill = "black",
                                                 shape = 21,
                                                 color = "transparent"))) +
  theme(legend.position = "left") +
  geom_text(data = points_pct,
            aes(label = pct)) +
  annotate(geom = "text",
           x = 0.75,
           y = 2.9,
           size = 2,
           label = expression(T[b]==0.09~"("~p==0.019~")"))

taxa_correlation <- shock %>%
  group_by(taxa_bin) %>%
  nest() %>%
  mutate(cor_test = map(.x = data, ~broom::tidy(cor.test(.x$MHW, .x$C19, method = "kendall")))) %>%
  select(taxa_bin, cor_test) %>%
  unnest(cor_test) %>%
  mutate(string = paste0("T[b]==",round(estimate, 3),"~(p==", round(p.value, 3), ")"))

mkt_correlation <- shock %>%
  group_by(mkt_bin) %>%
  nest() %>%
  mutate(cor_test = map(.x = data, ~broom::tidy(cor.test(.x$MHW, .x$C19, method = "kendall")))) %>%
  select(mkt_bin, cor_test) %>%
  unnest(cor_test) %>%
  mutate(string = paste0("T[b]==",round(estimate, 3),"~(p==", round(p.value, 3), ")"))

p2 <- base_plot +
  geom_density2d(color = "black",
                 linewidth = 0.1,
                 bins = 5) +
  geom_smooth(method = "lm", color = "red") +
  geom_point(aes(fill = taxa_bin,
                 size = mkt_bin)) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        panel.spacing = unit(0, "mm")) +
  facet_wrap(~taxa_bin, ncol = 1, as.table = F) +
  geom_text(data = taxa_correlation,
            x = 0, y = 2.5,
            aes(label = string),
            size = 1.5,
            parse = T)

p3 <- base_plot +
  geom_density2d(color = "black",
                 linewidth = 0.1,
                 bins = 5) +
  geom_smooth(method = "lm", color = "red") +
  geom_point(aes(fill = taxa_bin,
                 size = mkt_bin)) +
  scale_color_binned(type = "viridis") +
  theme(legend.position = "None",
        axis.title = element_blank(),
        panel.spacing = unit(0, "mm")) +
  facet_wrap(~mkt_bin, ncol = 1,
             as.table = F) +
  geom_text(data = mkt_correlation,
            x = 0, y = 2.5,
            aes(label = string),
            size = 1.5,
            parse = T)

p123 <- plot_grid(ggMarginal(p1, type = "densigram"),
                  p2, p3,
                 ncol = 3,
                 align = "h",
                 rel_widths = c(3.5, 1, 1),
                 axis = "b",
                 labels = "AUTO")

startR::lazy_ggsave(plot = p123,
                    filename = "figure2_concordance_of_shocks",
                    width = 18,
                    height = 10)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(shock,
        file = here("data", "output", "shock_estimates.rds"))


