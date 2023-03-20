# Basics

This is a set of R scripts that take data on grammatical features of Oceanic languages from Grambank and predicts what value those features would have in proto-languages given specific trees: Glottolog tree, Gray et al 2009-tree (MCCT) and Gray et al 2009-tree (aggregating of random sample of 100 from posterior). Particular attention is paid to Proto-Oceanic, Proto-Central Pacific, Proto-Polynesian and Proto-Eastern Polynesian. These predictions of the grammar of proto-languages are compared to findings in classical historical linguistics.

This is a document outlining the structure of this research project's code.

This project is entirely coded in R. The scripts are set up such that they can be called individually or from a makefile. Please note: they will *install required R packages* and make subdirectories for organising output.

# Location of data

-   The coding of individual languages are found here: `code/output/GB_wide/GB_wide_binarized.tsv`
-   The descriptions of features are found here: `code/output/GB_wide/parameters_binary.tsv`
-   The coding of proto-languages is also found her in a summarized form: `code/output/processed_data/HL_findings_for_comparison.tsv`
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
    
The git submodules need to be checked out at particular commits/tags in order to represent the same data as in the paper.

```
git -C grambank-analysed checkout v1.0

git -C dplace-data checkout v2.2.1
```

After this, it is advisable to run `git submodule update --init --recursive` again to make sure all is fetched correctly.

# General analysis workflow

1.  packages are installed and file path's defined (code/01_requirements.R)

2.  external data is fetched and wrangled (code/02\_\*)

3.  data coverage plots, densitree plot, example plot of specifically GB409 in Polynesian are generated and the trees are pruned for dialects and matches in GB (code/03\_\*)

4.  the parsimony analysis is run (code/\*04\_\*)

5.  the ML analysis is run (code/\*05\_\*)

6.  the ancestral state of the four proto-languages is extracted and the most common result is calculated (code/07\_\*)

7.  the mean values are taken for the posteriors results (code/\*08\_\*)

8.  comparison to HL predictions, including extra new predictions (code/09\_\*)

9.  certain supplementary tables are created (code/10\_\*)

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

# Notes on dialect aggregation

The datasets and the trees are aggregated such that the tips and unit of analysis are languages, not a dialects (given Glottolog's definitions). For the grambank dataset, I'm choosing the dialect with the least amount of missing data.

For the Gray et al 2009-tree I have hand picked which dialect to use if there is more than one based on data quality in ABVD (see 03_get_gray_tree_mcct.R and 03_process_gray_tree_posteriors.R). The remaining dialects are replaced by their language-leveled parent's glottocode.

# Note on tree pruning

For each reconstruction (each feature + tree + method), I'm pruning the relevant tree to only tips which have a value for that feature. I'm not inferring unknown tip states. I'm ignoring outcomes where more than half tips are missing.

# Integration with TeX and output locations

These scripts output TeX-formatted tables and plots which are in turn read into a tex-file which produces the paper itself. For example, `code/03_coverage_viz.R` makes a map plot of the data coverage of Oceanic languages (`coverage_map_oceanic.png`) and a TeX-table summarizing the coverage per island group (`island_groups_table.tex`). If this script is sourced in a place where there is a folder called "tex" which is sister-folder to "code", it will place these plots and tables here: "/tex/illustrations/plots_from_R". If this is not the case, the script will place these simply at "code/output". Either way, they will be generated. This file path is set in code/01_requirements.R.

# Requirements

1.  R should be installed. I haven't done thorough testing, but it should most likely be at least R 3.0.
2.  R packages. `code/01_requirements.R` will install R packages on your machine directly. It is called at the start of all analysis scripts.

# Feedback

If something isn't working well, please make sure that all the necessary files the scripts expects are indeed generated and are in the place the scripts expects them to be. Use the Makefile as a guide for the order of running scripts. If you are still encountering problems, feel free to get in touch. If you are an anonymous reviewer: either get in touch with me through the editor or leave comments regarding the failures in your review and I will see to them. It is my intention that anyone able to run scripts in R should be able to reproduce each step of analysis in this project (including fetching and wrangling data and summarizing results in tables and plots) on any regular personal computer.
