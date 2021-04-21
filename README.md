## Basics

This is a set of R scripts and one python scripts that take data on grammatical features of Oceanic languages from Grambank and predicts what value those features would have in proto-languages given two specific trees: Glottolog tree and Gray et al 2009-tree. Particular attention is paid to Proto-Oceanic, Proto-Central Pacific, Proto-Polynesian and Proto-Eastern Polynesian. These predictions of the grammar of proto-languages are compared to findings in classical historical linguistics and are used to computer convservatism scores for languages.

# To note
These scripts are set up to be run either from the command line or within Rstudio. The scripts are specifically fetching Grambank data as found in the clone of grambank/grambank-cldf.

These scripts will install python and R packages for you. You can see which ones here ([python](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code%20(R%20%26%20python)/requirements.txt)) and here ([R](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code%20(R%20%26%20python)/requirements.R)).

*  R should be installed. I haven't done testing, but it should most likely be at least R 3.0
*  python3 should be installed
*  Glottolog (CLDF), Glottolog (as data curation repos) and D-PLACE should all be downloaded from zenodo and placed in data/zenodo
  * If they are somewhere else, [config.json](https://github.com/HedvigS/Oceanic_computational_ASR/blob/main/code%20(R%20%26%20python)/config.json) should be updated to reflect that 
*  Grambank should exist as a cldf repos on the machine. Currently grambank cldf isn't public so I can't set it to grab from the web right now
  *  grambank-cldf is expected to live at "../../grambank-cldf/"
