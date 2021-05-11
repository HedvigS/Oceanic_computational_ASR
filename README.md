## Basics

This is a set of R scripts and one python script that take data on grammatical features of Oceanic languages from Grambank and predicts what value those features would have in proto-languages given two specific trees: Glottolog tree and Gray et al 2009-tree (MCCT). Particular attention is paid to Proto-Oceanic, Proto-Central Pacific, Proto-Polynesian and Proto-Eastern Polynesian. These predictions of the grammar of proto-languages are compared to findings in classical historical linguistics and are used to computer convservatism scores for languages.

### To note
These scripts are set up to be run either from the command line or within Rstudio. The scripts are specifically fetching Grambank data as found in the clone of grambank/grambank-cldf.

*Requirements*

*  R should be installed. I haven't done testing, but it should most likely be at least R 3.0
*  python3 should be installed
*  this project needs 4 datasets: Glottolog (CLDF), Glottolog (as data curation repos), D-PLACE and grambank-cldf
*  the firs three datasets (Glottolog (CLDF), Glottolog (as data curation repos) and D-PLACE) should all be downloaded from zenodo and placed in code/data/zenodo. [config.json](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code/config.json) specifies the precise locations and URLs for where to download the datasets. If you have them somewhere else on your machine, update config.json accordingly
* Grambank should exist as a cldf repos on the machine. Currently grambank cldf isn't public so only people with at least read-access to [glottobank/grambank-cldf](https://github.com/glottobank/grambank-cldf) can run this project. config.json specifies exactly where it expects grambank-cldf to live ("../../grambank-cldf/"), update accordingly if it lives elsewhere.

These scripts will install python and R packages for you. You can see which ones here ([python](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code/1_requirements.txt)) and here ([R](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code/1_requirements.R)).
