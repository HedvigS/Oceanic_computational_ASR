# Basics

This is a set of R scripts that take data on grammatical features of Oceanic languages from Grambank and predicts what value those features would have in proto-languages given specific trees: Glottolog tree, Gray et al 2009-tree (MCCT) and Gray et al 2009-tree (aggregating of random sample of 100 from posterior). Particular attention is paid to Proto-Oceanic, Proto-Central Pacific, Proto-Polynesian and Proto-Eastern Polynesian. These predictions of the grammar of proto-languages are compared to findings in classical historical linguistics.

This is a document outlining the structure of this research project's code.

This project is entirely coded in R. The scripts are set up such that they can be called individually and by Makefile rules. Please note: they will do the following on your local machine: a) *install required R packages* if they are not installed and b) make subdirectories for organising output.

# Location of data

-   The coding of individual languages are found here: `code/output/GB_wide/GB_wide_binarized.tsv`
-   The descriptions of features are found here: `code/output/GB_wide/parameters_binary.tsv`
-   The coding of proto-languages is also found her in a summarized form: `code/output/processed_data/HL_findings/HL_findings_for_comparison.tsv`
-   The datapoints where historical linguists disagree are found here: `code/data/HL_findings_conflicts.tsv`
-   The trees are found here: `code/output/processed_data/trees`
-   The grouping of languages into island groups are found here: `code/data/island_groups.tsv`
-   If you run `code/03_coverage_viz.R` you will generate tree plots and a summary TeX-table of the coverage stats.

# Data input
This project includes data from outside sources, namely: grambank, glottolog and D-PLACE. The full Git project includes git submodules for the following dataset: grambank-analysed and dplace-data. The git repository for grambank-analysed in turn includes git submodules for glottolog-cldf and grambank. See below for versions and Zenodo and GitHub locations:

Zenodo locations:

*   Grambank-analysed (v1.0) <https://doi.org/10.5281/zenodo.7740822>
*   Grambank (v.1.0) <https://doi.org/10.5281/zenodo.7740140>
*   glottolog-cldf (v4.5) <https://doi.org/10.5281/zenodo.5772642>
*   dplace-data (v2.2.1) <https://doi.org/10.5281/zenodo.5554395>

GitHub locations:
* Grambank-analysed (v1.0) <https://github.com/grambank/grambank-analysed/tree/v1.0>
    -  Grambank (v1.0) <https://github.com/grambank/grambank/tree/v1.0>
    -  glottolog-cldf (v4.5) <https://github.com/glottolog/glottolog-cldf/tree/v4.5>
* dplace-data (v2.2.1) <https://github.com/D-PLACE/dplace-data/tree/v2.2.1>

It can be difficult to set-up git submodules. Because of this reason, and the above mentioned access-restrictions, I have already prepped all the data necessary  for this particular studdy and placed in the following directories: `code/output/processed_data` and `code/output/GB_wide`. If you have access to all the relevant git submodules you can create these by running `make get_external` in the directory code. This will execute a rule in the makefile that generates the necesary files based on external data.

The necessary code for updating and initialising the submodules is:

    `git submodule update --init --recursive`
    
The flag `--recursive` means that all the submodules are initialised, including submodules within submodules. This means that all of AUTOTYP-data, glottolog-cldf, WALS etc are cloned to the local machine since they are submodules of `grambank-analysed`.
    
The git submodules need to be checked out at particular commits/tags in order to represent the same data as in the paper. The version here at origin/main has the submodules checked out correctly. You can explicitly check them out by running:

```
git -C grambank-analysed checkout v1.0

git -C dplace-data checkout v2.2.1
```

After this, it is advisable to run `git submodule update --init --recursive` again to make sure all is fetched correctly.

In addition, `02_get_zenodo_dirs.R` will download the data from Zenodo-URLs if the git submodules aren't working.

# General analysis workflow

1.  packages are installed and file path's defined (code/01_requirements.R)

2.  external data is fetched and wrangled (code/02\_\*)

3.  data coverage plots, densitree plot, example plot of specifically GB409 in Polynesian are generated and the trees are pruned for dialects and matches in GB (code/03\_\*)

4.  the parsimony analysis is run (code/\*04\_\*)

5.  the ML analysis is run (code/\*05\_\*)

6.  the ancestral state of the four proto-languages is extracted and the most common result is calculated (code/07\_\*)

7.  the mean values are taken for the posteriors results (code/\*08\_\*)

8.  comparison to HL predictions, including extra new predictions (code/09\_\*)

9.  D-estimate analysis (code/10\_\*)

10.  certain supplementary tables are created (code/11\_\*)

# Running the analysis

The R-scripts of this project are organised to be run in a particular order. The Makefile has rules that enable this. The Makefile rule `all_excl_external` runs all the analysis, but does not require access to external git reposes (see section above).

It is also possible to call on each script in turn, see the Makefile for exact appropriate order.

Description of makefile rules. Because the analysis on the 100 sample of the posterior takes a while it is possible to run part of the analysis excluding this.

-   `analysis_excl_posteriors` runs the Parsimony and ML analysis for the glottolog tree and the MCCT-tree from Gray et al (2009). It does *not* run the analysis on the posteriors and it does not fetch external data.
-   `all_excl_external_incl_posteriors` runs the Parsimony and ML analysis for the glottolog tree, the MCCT-tree from Gray et al (2009) and the posterior sample. It does not fetch external data. It does including summing up of the analysis with tables and plots.
-   `all_incl_external` runs all analysis, including featching external data

# Beware

The scripts will install R-packages if they are not already installed (see code/01_requirements.R for exact list).

All analysis is possible to run on a personal computer, cluster access it not necessary. Running all the analysis, including all the 100 posterior trees, should take less than 30 hours. I advice you to get it going on Friday afternoon and reap the rewards by Monday morning.

# List of content in the folder "code"

**Makefile**
Makefile - Makefile with rules  for running analysis that can be executed from command line. See section *Running the analysis* above.

**Processing, analysis, visualisation and  results**
01_check_used_functions.R - checks what functions are being used in all R-scripts and makes files for tex to use in order to cite every package appropriately
01_requirements.R - installs and loads packages, creates output dirs if necessary and establishes certain stable variables called on later by other scripts
02_get_zenodo_dirs.R - gets data-sets from Zenodo
02_get_glottolog_language_table.R - creates a table of languiods from glottolog-cldf
02_get_grambank_data.R - creates necessary grambank files, using Grambank-analysed scripts. Sets working directory to inside Grambank-analysed and then sets it back.
02_massage_HL_findings_sheets.R - manipulates the sheets of coding of proto-languages based on classical linguistics
analysis_scripts_gray_all_posterior/03_process_gray_tree_posteriors.R - samples 100 trees from the 
posterior, checks for non-binary splits, prunes to dataset, change tips to glottocodes etc.
analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R - reads in mcct tree, prunes to dataset, change tips to glottocodes etc.
03_prune_glottolog_tree.R - prunes and manipulates glottolog trees
03_compare_dists.R - compares the patristic distances between trees
03_coverage_viz.R - makes plots to illustrate data coverage
03_densitree.R - makes plot to show densities of posteriors
03_polynesian_viz.R - makes plot to exemplify Polynesian languages for feature GB409
03_tree_heatmap.R - makes plot of tree heat map (Gray et al 2009 MCCT
04_ASR_parsimony_glottolog.R - runs analysis of MP on glottolog tree
05_ASR_ML_glottolog.R - runs analysis of ML on glottolog-tree

07a_get_ancestral_states_parsimony_glottolog.R - fetches the specific ancestral states given MP on the glottolog tree
07b_get_ancestral_states_ML_glottolog.R - - fetches the specific ancestral states given ML on the glottolog tree

analysis_scripts_gray_mcct/04_ASR_parsimony_gray_mcct.R - runs analysis of MP on gray et al 2009 MCCT
analysis_scripts_gray_mcct/05_ASR_ML_gray_mcct.R  - runs analysis of ML on gray et al 2009 MCCT
analysis_scripts_gray_mcct/07a_get_ancestral_states_parsimony_mcct.R - fetches the specific ancestral states given MP on the gray et al 2009 MCCT
analysis_scripts_gray_mcct/07b_get_ancestral_states_ML_mcct.R - fetches the specific ancestral states given ML on the gray et al 2009 MCCT


analysis_scripts_gray_all_posterior/04_ASR_parsimony_gray_posteriors.R - runs analysis of MP on gray et al 2009 posteriors
analysis_scripts_gray_all_posterior/05_ASR_ML_gray_posteriors.R - runs analysis of ML on gray et al 2009 posteriors
analysis_scripts_gray_all_posterior/07a_get_ancestral_states_parsimony_gray_posteriors.R- fetches the specific ancestral states given MP on the gray et al 2009 posteriors
analysis_scripts_gray_all_posterior07b_get_ancestral_states_ML_gray_posteriors.R
analysis_scripts_gray_all_posterior/08_aggregate_posteriors_reconstructions.R

07d_get_ancestral_states_most_common.R - - fetches the specific ancestral states given MC (disregards trees)

08_aggregate_all_reconstructions.R - combines all the ancestral states from all methods and all trees 
09a_compare_to_HL.R - compares the output of 08_aggregate_all_reconstructions.R to the classical HL coding (non-conflicts)
09b_compare_to_HL_conflicts.R - compares the output of 08_aggregate_all_reconstructions.R to the classical HL coding specifically when HLs disagree
09c_distances_between_all_reconstructions.R - compares the Gower-distances between all reoncstructions (including HL)
09d_extra_predictions.R - compiles a table of reconstructions from MP, ML and MC for features that don't have a match to HL
09e_compare_scores_against_props.R - compares the reconstruction agreement to the proportion of values that are in the most common state
10a_phylo_d_investigation.R - script that investigates some perculariies of caper::phylo.d
10b_get_phylo_D_estimate.R - gets phylo-d score for all features and all trees
10c_aggregate_phylo_D_estimate.R - aggregates dat from 10b_get_phylo_D_estimate.R 
10d_phylo_d_estimate_plotting.R - plots from 10c_aggregate_phylo_D_estimate.R 
11_make_supp_tables.R - makes nice latex tables for supplementary material

**Functions**

fun_custom_parsimony_results_table.R - functions for making parsimony results table
fun_def_get_zenodo.R - function for downloading Zenodo data
fun_def_h_load.R - function  for checking packages are installed and if not installs them
fun_def_list.functions.R - modified version of NCmisc::list.functions.in.file that reports all functions (doesn't run un ique())
fun_def_plotRECON_tweaked.R - modified version of corHMM::plotRECON with some aesthetics options (e.g. removing margins)
fun_get_ASR_nodes.R - function for fetching ancestral states
fun_keep_as_tip.R - modified function of ape::keep.tip for internal nodes

**misc**
tex_check_refs.R - script to check references used in the tex-document
config.json - json of file locations


**Folders**

output - all output. All contents of this folder can be deleted and then regenerated by running the scripts
data - input data that is not fetched from Zenodo
analysis_scripts_gray_mcct - R-scripts specifically dealing with the Gray et al 2009-MCCT
analysis_scripts_gray_all_posterior - - R-scripts specifically dealing with the Gray et al 2009 posteriors

# Notes on dialect aggregation

The datasets and the trees are aggregated such that the tips and unit of analysis are languages, not a dialects (given Glottolog's definitions). For the grambank dataset, I'm choosing the dialect with the least amount of missing data.

For the Gray et al 2009-tree I have hand-picked which dialect to use if there is more than one based on data quality in ABVD (see [list here for duplciates to drop](https://github.com/HedvigS/Oceanic_computational_ASR/blob/aec63bdc1e30f94ecaca4158ab4d2afbb4c59585/code/01_requirements.R#L227)). The remaining dialects are replaced by their language-leveled parent's glottocode.

# Note on tree pruning

For each reconstruction (each feature + tree + method), I'm pruning the relevant tree to only tips which have a value for that feature. I'm not inferring unknown tip states. I'm ignoring outcomes where more than half tips are missing.

# Integration with TeX and output locations

These scripts output TeX-formatted tables and plots which are in turn read into a tex-file which produces the paper itself. For example, `code/03_coverage_viz.R` makes a map plot of the data coverage of Oceanic languages (`coverage_map_oceanic.png`) and a TeX-table summarizing the coverage per island group (`island_groups_table.tex`). If this script is sourced in a place where there is a folder called "tex" which is sister-folder to "code", it will place these plots and tables here: "/tex/illustrations/plots_from_R". If this is not the case, the script will place these simply at "code/output". Either way, they will be generated. This file path is set in code/01_requirements.R.

# Requirements

1.  R should be installed. I haven't done thorough testing, but it should most likely be at least R 3.0.
2.  R packages. `code/01_requirements.R` will install and load R packages on your machine directly. It is called at the start of all analysis scripts.

# Feedback

If something isn't working well, please make sure that all the necessary files the scripts expects are indeed generated and are in the place the scripts expects them to be. Use the Makefile as a guide for the order of running scripts. If you are still encountering problems, feel free to get in touch. If you are an anonymous reviewer: either get in touch with me through the editor or leave comments regarding the failures in your review and I will see to them. It is my intention that anyone able to run scripts in R should be able to reproduce each step of analysis in this project (including fetching and wrangling data and summarizing results in tables and plots) on any regular personal computer.
