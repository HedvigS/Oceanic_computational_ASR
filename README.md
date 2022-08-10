# Basics
This is a set of R scripts that take data on grammatical features of Oceanic languages from Grambank and predicts what value those features would have in proto-languages given specific trees: Glottolog tree, Gray et al 2009-tree (MCCT) Gray et al 2009-tree (random sample of 100 from posterior). Particular attention is paid to Proto-Oceanic, Proto-Central Pacific, Proto-Polynesian and Proto-Eastern Polynesian. These predictions of the grammar of proto-languages are compared to findings in classical historical linguistics.

This is a document outlining the structure of this research project's code.

This project is entirely coded in R. The scripts are set up such that they can be called individually or from a makefile. Please note: they will *install required R packages* and make subdirectories for organising output.

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

## Notes on dialect aggregation
The datasets and the trees are aggregated such that the tips and unit of analysis are languages, not a dialects (given Glottolog's definitions). For the grambank dataset, I'm choosing the dialect with the least amount of missing data. 

For the Gray et al 2009-tree I have hand picked which dialect to use if there is more than one based on data quality in ABVD (see 03_get_gray_tree_mcct.R and 03_process_gray_tree_posteriors.R).

## Note on Gray et al 2009-tree
I'm using the all of the posterior trees and the MCCT-tree. I'm using the lexibank/ABVD taxa file rather than D-PLACE/dplace-data. 

Because this takes a signifiant amount of time (there are 4200 trees), I have also stored scripts in this repos which just run over the MCCT tree. These can be found in the directory "analysis_scripts_gray_mcct". They can be used as a demo of sorts for the full analysis. For the phangrorn::acctran() function which is used for calcuating parsimony cost from root to tip, it is necessary that the tree is binary. I used ape::multi2di() to binarise the MCCT-tree in this instance. Because branch lengths matters less for parsimony than for ML, this is not as much of a concern as it otherwise would be - but it is still slightly different from the ASR-function castor::asr_max_parsimony() which is used to estimate previous states.

The full analysis on all the posteriors is in "analysis_scripts_gray_all_posterior".

## Note on tree pruning (general)
For each reconstruction, I'm pruning the relevant tree to only tips which have a value for that feature. I'm not inferring unknown tip states. I could, but I am not. Again, I think principle-wise it'd be a bit too much for traditional linguists to stomach and it shouldn't make much of a difference. For the rest of the results, I'm ignoring trees where more than half tips are missing.

## Basics


### To note
These scripts are set up to be run either from the command line or within Rstudio. The scripts are specifically fetching Grambank data as found in the clone of grambank/grambank-cldf.

*Requirements*

*  R should be installed. I haven't done testing, but it should most likely be at least R 3.0
*  python3 should be installed
* packages
  * R packages. These scripts will install R packages on your machine directly. You can see which ones [here](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code/1_requirements.R). 
  * Python packages. You will also need additional packages for python, but as per python convention you are to install those yourself (in whichever environment you'll be running the code). The list of those are [here](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code/1_requirements.txt). You can also uncomment `python3 -m pip install -r 1_requirements.txt` in the shell script  `run_entire_project.sh`.
* datasets
  * this project needs four datasets: Glottolog (CLDF), Glottolog (as data curation repos), D-PLACE and grambank-cldf
  * the first three datasets (Glottolog (CLDF), Glottolog (as data curation repos) and D-PLACE) should all be downloaded from zenodo, placed in code/data/zenodo and unzipped. [config.json](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code/config.json) specifies the precise locations and URLs for where to download the datasets. If you have them somewhere else on your machine, update config.json accordingly
  * Grambank should exist as a cldf repos on the machine. Currently grambank cldf isn't public so only people with at least read-access to [glottobank/grambank-cldf](https://github.com/glottobank/grambank-cldf) can run this project fully. config.json specifies exactly where it expects grambank-cldf to live ("../../grambank-cldf/"), update accordingly if it lives elsewhere.

For more nitty-gritty details on the code, see [the code dir README](https://github.com/HedvigS/Oceanic_computational_ASR/tree/main/code#readme).


# Git submodules

    git submodule update --init --recursive

