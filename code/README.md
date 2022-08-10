## Basics
This is a document outlining the structure of this research project's code.

This project is entirely coded in R. The scripts are set up such that they can be called individually or from a makefile. They will install packages and make subdirectories for organising output.

# Location of data
* The coding of individual languages are found here: `code/output/GB_wide/GB_wide_binarized.tsv`
* The descriptions of features are found here: `code/output/GB_wide/parameters_binary.tsv`
* The coding of proto-languages is also found here with certain more detail: `code/output/processed_data/HL_findings_for_comparison.tsv`
* The trees are found here: `code/output/processed_data/trees`
* The datapoints where historical linguists disagree are found here: `code/data/HL_findings_conflicts.tsv`
* The grouping of languages into island groups are found here: `code/data/island_groups.tsv`

# Concerning external data
This project includes data from outside sources, namely: grambank, glottolog and D-PLACE. The full project includes git submodules for the following dataset: grambank-analysed and dplace-data. The git repository for grambank-analysed in turn includes git submodules for glottolog-cldf and grambank, which this project also make use of (grambank-analysed also includes git submodules for autotyp-data and wals, but these are not used in this project).

Grambank 1.0 is not publicly released yet. Until it is, the full grambank dataset is not accessible publicly. For this project, I have gotten approval to share the subset of grambank which concerns Oceanic languages specifically. The script code/02_get_grambank_data.R creates the appropriate table for this project and places it in a subdirectory accessible to you.

It can be difficult to set-up git submodules. Because of this reason, and the above mentioned access-restrictions, I have already prepped all the data necessary and placed in the following directories: `code/output/processed_data` and `code/output/GB_wide`. If you have access to all the relevant git submodules you can create these by running the rule `get_external` in the makefile.



Each of these things are over three methods and two trees.

Methods: parsimony, ML and SCM.
Trees: Gray et al 2009-tree and Glottolog (Glottolog tree only computed for SM, not main goal but still being run).

## General note on state of project
This project is based on a chapter of my PhD thesis and work with Simon, Cara and Hannah on stability of features. Most of the code is written by me, with some of Simon's code. The biggest difference from the PhD chapter is that I'm looping over all the trees in the posterior rather than using the summary tree for the Gray et a 2009-tree. There are also some smaller changes for the ML ASR; I'm using corHMM::corHMM instead of corHMM::rayDISC (not big change honestly), I'm no longer having ambiguous states at tips and root.p has changed from "NULL" to "yang". Because the scripts are currently a mix of current Hedvig, past Hedvig and Simon sometimes things are done in a bit of a funny manner (particularly the way results.csv are build for each tree and method). I have on my to do list to smooth that out, it's just taken backseat to things running correctly and incorporating SCM. 

You are welcome to do PR's if you see anything you want to chip in on.

I'm hopefully presenting based on this work both at ICAL and COOL this year, and planning on submitting an article as well before the years end.

## Note on Grambank dataset
I'm using version 1, found at grambank-cldf. The dataset is binarised for this project.

## Notes on dialect aggregation
The datasets and the trees are aggregated such that the tips and unit of analysis are languages, not a dialects. For the grambank dataset, I'm merging dialects into on language and randomly picking a value if they have conflicting values for the same feature (see 2_get_grambank_data.R). If one dialect has missing data for a feature and another has a defined value, I go with the non-missing data. In some cases, this results in slightly different coding for each run since the values are randomly picked when there is more than one for the same language.

For the Gray et al 2009-tree I have hand picked which dialect to use if there is more than one based on data quality in ABVD, see 3_get_gray_tree.R for details. I'm not using the taxa file for this tree as found at D-PLACE/dplace-data, but instead the one found at lexibank/ABVD ([see this PR for discussion](https://github.com/D-PLACE/dplace-data/pull/293)). The Glottolog tree is created pulled form glottolog-cldf.

## Note on Gray et al 2009-tree
I'm using the all of the posterior trees and the MCCT-tree. I'm using the lexibank/ABVD taxa file rather than D-PLACE/dplace-data. 

Because this takes a signifiant amount of time (there are 4200 trees), I have also stored scripts in this repos which just run over the MCCT tree. These can be found in the directory "analysis_scripts_gray_mcct". They can be used as a demo of sorts for the full analysis. For the phangrorn::acctran() function which is used for calcuating parsimony cost from root to tip, it is necessary that the tree is binary. I used ape::multi2di() to binarise the MCCT-tree in this instance. Because branch lengths matters less for parsimony than for ML, this is not as much of a concern as it otherwise would be - but it is still slightly different from the ASR-function castor::asr_max_parsimony() which is used to estimate previous states.

The full analysis on all the posteriors is in "analysis_scripts_gray_all_posterior".

## Note on tree pruning (general)
For each reconstruction, I'm pruning the relevant tree to only tips which have a value for that feature. I'm not inferring unknown tip states. I could, but I am not. Again, I think principle-wise it'd be a bit too much for traditional linguists to stomach and it shouldn't make much of a difference. For the rest of the results, I'm ignoring trees where more than half tips are missing.
