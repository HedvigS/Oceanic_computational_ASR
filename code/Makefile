# Determine this makefile's path.
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

install_pkgs:
	Rscript 01_requirements.R 

get_external:
	@$(MAKE) -f $(THIS_MAKEFILE) install_pkgs
	Rscript 02_get_glottolog_language_table.R
	Rscript 02_get_grambank_data.R 
	Rscript 02_massage_HL_findings_sheets.R
	Rscript analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R #pruning tree and changing tip names to glottocodes. removing duplicates and merges dialects
	Rscript 02_prune_glottolog_tree.R

prep:

Rscript 03_coverage_viz.R


Rscript analysis_scripts_gray_mcct/04_ASR_parsimony_gray_mmct.R
Rscript 04_ASR_parsimony_glottolog.R
Rscript analysis_scripts_gray_mcct/05_ASR_ML_gray_mcct.R
Rscript 05_ASR_ML_glottolog.R
Rscript 07a_get_ancestral_states_parsimony_glottolog.R
Rscript 07b_get_ancestral_states_ML_glottolog.R
Rscript analysis_scripts_gray_mcct/07a_get_ancestral_states_parsimony_mcct.R
Rscript analysis_scripts_gray_mcct/07b_get_ancestral_states_ML_mcct.R

	
	



