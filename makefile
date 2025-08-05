# Shortcuts
# Content directories
FIGURES=results/img/
TABLES=results/tab/
# Data directories
OUTPUT=data/output/
PROCESSED=data/processed/
# Code directories
ANALYSIS=scripts/02_analysis/
CONTENT=scripts/03_content/

all: figures tables
tables: $(TABLES)tab1_main_model.tex $(TABLES)tab2_drivers.tex $(TABLES)tabS2_robustness_checks.tex
figures: $(FIGURES)fig1_map.pdf $(FIGURES)fig2_ts_plot.pdf $(FIGURES)figure3_metacategories.pdf $(FIGURES)figure4_concordance_of_shocks.pdf results/img/figure5_concordance_vs_diversity.pdf $(FIGURES)s3.pdf $(FIGURES)s4.pdf
input_data: $(PROCESSED)year_eu_spp.rds $(PROCESSED)year_eu.rds $(PROCESSED)characteristics.rds
dag: workflow.png

# Tables -----------------------------------------------------------------------
$(TABLES)tab1_main_model.tex: $(ANALYSIS)01_fit_mixed_effects_model.R $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(TABLES)tab2_drivers.tex: $(ANALYSIS)04_drivers_of_resilience.R data/output/shock_estimates.rds
	cd $(<D);Rscript $(<F)

$(TABLES)tabS2_robustness_checks.tex: $(ANALYSIS)01b_robustness.R $(PROCESSED)year_eu.rds $(OUTPUT)mixed_effects_model.rds
	cd $(<D);Rscript $(<F)

$(TABLES)taxa_table.tex: $(CONTENT)02_taxa_tabvle.R
	cd $(<D);Rscript $(<F)

# Figures ----------------------------------------------------------------------
$(FIGURES)fig1_map.pdf: $(CONTENT)/01_map.R
	cd $(<D);Rscript $(<F)

$(FIGURES)fig2_ts_plot.pdf: $(ANALYSIS)02_differences_by_period.R $(OUTPUT)mixed_effects_model.rds $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(FIGURES)figure3_metacategories.pdf: $(CONTENT)/03_metacategories.R data/raw/Histograma_de_metacategorias.xlsx
	cd $(<D);Rscript $(<F)

$(FIGURES)figure4_concordance_of_shocks.pdf: $(ANALYSIS)03_concordance_analysis.R $(OUTPUT)coefs.rds $(PROCESSED)characteristics.rds $(OUTPUT)period_diffs.rds
	cd $(<D);Rscript $(<F)

$(FIGURES)figure5_concordance_vs_diversity.pdf: $(ANALYSIS)05_concordance_vs_diversity.R $(OUTPUT)shock_estimates.rds
	cd $(<D);Rscript $(<F)


# Supplementary figures
$(FIGURES)s1.pdf: $(ANALYSIS)01b_robustness.R $(PROCESSED)year_eu.rds $(OUTPUT)mixed_effects_model.rds
	cd $(<D);Rscript $(<F)

$(FIGURES)s2.pdf: $(ANALYSIS)01b_robustness.R $(PROCESSED)year_eu.rds $(OUTPUT)mixed_effects_model.rds
	cd $(<D);Rscript $(<F)

$(FIGURES)s3.pdf: $(CONTENT)/04_diagnostic_plots.R $(OUTPUT)mixed_effects_model.rds $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(FIGURES)s4.pdf: $(CONTENT)/04_diagnostic_plots.R $(OUTPUT)mixed_effects_model_with_random_intercept.rds $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

# Output data ------------------------------------------------------------------
$(OUTPUT)mixed_effects_model.rds: $(ANALYSIS)01_fit_mixed_effects_model.R $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(OUTPUT)mixed_effects_model_with_random_intercept.rds: $(ANALYSIS)01b_robustness.R $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(OUTPUT)coefs.rds: $(ANALYSIS)01_fit_mixed_effects_model.R $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(OUTPUT)period_diffs.rds: $(ANALYSIS)02_differences_by_period.R $(OUTPUT)mixed_effects_model.rds $(PROCESSED)year_eu.rds
	cd $(<D);Rscript $(<F)

$(OUTPUT)shock_estimates.rds: $(ANALYSIS)03_concordance_analysis.R $(OUTPUT)coefs.rds $(PROCESSED)characteristics.rds $(OUTPUT)period_diffs.rds
	cd $(<D);Rscript $(<F)

# Input data -------------------------------------------------------------------
$(PROCESSED)year_eu_spp.rds: scripts/01_build_data/01_build_data.R
	cd $(<D);Rscript $(<F)

$(PROCESSED)year_eu.rds: scripts/01_build_data/01_build_data.R
	cd $(<D);Rscript $(<F)

$(PROCESSED)characteristics.rds: scripts/01_build_data/01_build_data.R
	cd $(<D);Rscript $(<F)

# DAG
workflow.png: Makefile
		LANG=C make -pBnd | python3 make_p_to_json.py | python3 json_to_dot.py | dot -Tpng -Gdpi=300 -o workflow.png
