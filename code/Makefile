# Determine this makefile's path.
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

all_excl_external: install_pkgs other_prep parsimony ML agg_posteriors most_common compare_HL wrapping_up

all_incl_external: get_external all_excl_external

install_pkgs:
	Rscript 01_requirements.R 

get_external:
	@$(MAKE) -f $(THIS_MAKEFILE) install_pkgs
	Rscript 02_get_glottolog_language_table.R
	Rscript 02_get_grambank_data.R 
	Rscript 02_massage_HL_findings_sheets.R
	Rscript 02_prune_glottolog_tree.R
	Rscript analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R #pruning tree and changing tip names to glottocodes. removing duplicates and merges dialects
	Rscript analysis_scripts_gray_all_posterior/03_process_gray_tree_posteriors.R

other_prep:
	Rscript 03_coverage_viz.R
	Rscript 03_densitree.R
	Rscript 03_polynesian_viz.R

parsimony:
	#run analysis
	Rscript 04_ASR_parsimony_glottolog.R
	Rscript analysis_scripts_gray_mcct/04_ASR_parsimony_gray_mmct.R
	Rscript analysis_scripts_gray_all_posterior/04_ASR_parsimony_gray_posteriors.R
	
	#extract states for specific nodes
	Rscript 07a_get_ancestral_states_parsimony_glottolog.R
	Rscript analysis_scripts_gray_mcct/07a_get_ancestral_states_parsimony_mcct.R
	Rscript analysis_scripts_gray_all_posterior/07a_get_ancestral_states_parsimony_gray_posteriors.R
	
ML: 
	#run analysis
	Rscript 05_ASR_ML_glottolog.R
	Rscript analysis_scripts_gray_mcct/05_ASR_ML_gray_mcct.R
	Rscript analysis_scripts_gray_all_posterior/05_ASR_ML_gray_posteriors.R

	#extract states for specific nodes
	Rscript 07b_get_ancestral_states_ML_glottolog.R
	Rscript analysis_scripts_gray_mcct/07b_get_ancestral_states_ML_mcct.R
	Rscript analysis_scripts_gray_all_posterior/07b_get_ancestral_states_ML_gray_posteriors.R

agg_posteriors:
	Rscript analysis_scripts_gray_all_posterior/08_aggregate_posteriors_reconstructions.R

most_common:	
	Rscript 07d_get_ancestral_states_most_common.R
	
compare_HL: 
	Rscript 09a_compare_to_HL.R
	Rscript 09b_compare_to_HL_conflicts.R
	Rscript 09c_distances_between_reconstructions.R
	Rscript 9d_compare_method_rankings.R
	
wrapping_up:
	Rscript 09e_extra_predictions.R
	Rscript 10_make_supp_tables.R
