# Basics

This is a set of R scripts that take data on grammatical features of Oceanic languages from Grambank and predicts what value those features would have in proto-languages given specific trees: Glottolog tree, Gray et al 2009-tree (MCCT) and Gray et al 2009-tree (aggregating of random sample of 100 from posterior). Particular attention is paid to Proto-Oceanic, Proto-Central Pacific, Proto-Polynesian and Proto-Eastern Polynesian. These predictions of the grammar of proto-languages are compared to findings in classical historical linguistics.

This is a document outlining the structure of this research project's code.

This project is entirely coded in R. The scripts are set up such that they can be called individually or from a makefile. Please note: they will *install required R packages* and make subdirectories for organising output.

# Location of data

-   The coding of individual languages are found here: `code/output/GB_wide/GB_wide_binarized.tsv`
-   The descriptions of features are found here: `code/output/GB_wide/parameters_binary.tsv`
-   The coding of proto-languages is also found here with certain more detail: `code/output/processed_data/HL_findings_for_comparison.tsv`
-   The datapoints where historical linguists disagree are found here: `code/data/HL_findings_conflicts.tsv`
-   The trees are found here: `code/output/processed_data/trees`
-   The grouping of languages into island groups are found here: `code/data/island_groups.tsv`
-   If you run `code/03_coverage_viz.R` you will generate tree plots and a summary TeX-table of the coverage stats.

# Concerning external data

This project includes data from outside sources, namely: grambank, glottolog and D-PLACE. The full project includes git submodules for the following dataset: grambank-analysed and dplace-data. The git repository for grambank-analysed in turn includes git submodules for glottolog-cldf and grambank, which this project also make use of (grambank-analysed also includes git submodules for autotyp-data and wals, but these are not used in this project).

Grambank 1.0 is not publicly released yet. Until it is, the full grambank dataset is not accessible publicly. For this project, I have gotten approval to share the subset of grambank which concerns Oceanic languages specifically. The script `code/02_get_grambank_data.R` creates the appropriate table for this project and places it in a subdirectory accessible to you.

It can be difficult to set-up git submodules. Because of this reason, and the above mentioned access-restrictions, I have already prepped all the data necessary and placed in the following directories: `code/output/processed_data` and `code/output/GB_wide`. If you have access to all the relevant git submodules you can create these by running the rule `get_external` in the makefile.

The necessary code for updating submodules is:

    git submodule update --init --recursive

# Running the analysis

The R-scripts of this project are organised to be run in a particular order. The Makefile has rules that enable this. The Makefile rule `all_excl_external` runs all the analysis, but does not require access to external git reposes (see section above).

It is also possible to call on each script in turn, see the Makefile for exact appropriate order.

# Beware

The scripts will install R-packages if they are not already installed. All analysis is possible to run on a personal computer, cluster access it not necessary. Running all the analysis, including all the 100 posterior trees, should take less than 30 hours.

# Notes on dialect aggregation

The datasets and the trees are aggregated such that the tips and unit of analysis are languages, not a dialects (given Glottolog's definitions). For the grambank dataset, I'm choosing the dialect with the least amount of missing data.

For the Gray et al 2009-tree I have hand picked which dialect to use if there is more than one based on data quality in ABVD (see 03_get_gray_tree_mcct.R and 03_process_gray_tree_posteriors.R). The remaining dialects are replaced by their language-leveled parent's glottocode.

# Note on tree pruning

For each reconstruction (each feature + tree + method), I'm pruning the relevant tree to only tips which have a value for that feature. I'm not inferring unknown tip states. I'm ignoring outcomes where more than half tips are missing.

# Requirements

-   R should be installed. I haven't done thourough testing, but it should most likely be at least R 3.0
-   packages
    -   R packages. code/01_requirements.R will install R packages on your machine directly. It is called at the start of all subsequent scripts.
