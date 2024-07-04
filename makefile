all: figures tables
tables: results/tab/tab1_main_model.png results/img/tab2_drivers.png results/tab/tabS2_robustness_checks.png
figures: results/img/fig1_map.png results/img/fig2_ts_plot.png results/img/figure3_concordance_of_shocks.png results/img/figure5_concordance_vs_diversity.png
output: data/output/mixed_effects_model.rds data/output/coefs.rds data/output/period_diffs.rds data/output/shock_estimates.rds
input_data: data/processed/year_eu_spp.rds data/processed/year_eu.rds data/processed/characteristics.rds

# Tables -----------------------------------------------------------------------
results/tab/tab1_main_model.png: scripts/02_analysis/01_fit_mixed_effects_model.R data/processed/year_eu.rds
	cd $(<D);Rscript $(<F)

results/img/tab2_drivers.png: scripts/02_analysis/04_drivers_of_resilience.R data/output/shock_estimates.rds
	cd $(<D);Rscript $(<F)

results/tab/tabS2_robustness_checks.png: scripts/02_analysis/01b_robustness.R data/processed/year_eu.rds data/output/mixed_effects_model.rds
	cd $(<D);Rscript $(<F)

# Figures ----------------------------------------------------------------------
results/img/fig1_map.png: scripts/03_content/01_map.R
	cd $(<D);Rscript $(<F)

results/img/fig2_ts_plot.png: scripts/02_analysis/02_differences_by_period.R data/output/mixed_effects_model.rds data/processed/year_eu.rds
	cd $(<D);Rscript $(<F)

results/img/figure3_concordance_of_shocks.png: scripts/02_analysis/03_concordance_analysis.R data/output/coefs.rds data/processed/characteristics.rds data/output/period_diffs.rds
	cd $(<D);Rscript $(<F)

results/img/figure4_metacategories.png: scripts/03_content/03_metacategories.R data/raw/Histograma de metacategorías.xlsx
	cd $(<D);Rscript $(<F)

results/img/figure5_concordance_vs_diversity.png: scripts/02_analysis/05_concordance_vs_diversity.R data/output/shock_estimates.rds
	cd $(<D);Rscript $(<F)

# Output data ------------------------------------------------------------------
data/output/mixed_effects_model.rds: scripts/02_analysis/01_fit_mixed_effects_model.R data/processed/year_eu.rds
	cd $(<D);Rscript $(<F)

data/output/coefs.rds: scripts/02_analysis/01_fit_mixed_effects_model.R data/processed/year_eu.rds
	cd $(<D);Rscript $(<F)

data/output/period_diffs.rds: scripts/02_analysis/02_differences_by_period.R data/output/mixed_effects_model.rds data/processed/year_eu.rds
	cd $(<D);Rscript $(<F)

data/output/shock_estimates.rds: scripts/02_analysis/03_concordance_analysis.R data/output/coefs.rds data/processed/characteristics.rds data/output/period_diffs.rds
	cd $(<D);Rscript $(<F)

# Inptue data ------------------------------------------------------------------
data/processed/year_eu_spp.rds: scripts/01_build_data/01_build_data.R
	cd $(<D);Rscript $(<F)

data/processed/year_eu.rds: scripts/01_build_data/01_build_data.R
	cd $(<D);Rscript $(<F)

data/processed/characteristics.rds: scripts/01_build_data/01_build_data.R
	cd $(<D);Rscript $(<F)
