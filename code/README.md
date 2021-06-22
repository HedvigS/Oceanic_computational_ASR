## Basics
This is a markdown file with some method notes concerning the ancestral state reconstruction, primarily for Simon and Ben. See README one level up for basics on requirements, datasets etc.

I'm doing three things in this project
a) compare computational ASR conclusions about structural features with HL findings
b) compute rates of change for features
c) compute conservatism rates of languages

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

For the Gray et al 2009-tree I have hand picked which dialect to use if there is more than one based on data quality in ABVD, see 3_get_gray_tree.R for details. I'm not using the taxa file for this tree as found at D-PLACE/dplace-data, but instead the one found at lexibank/ABVD ([see this PR for discussion](https://github.com/D-PLACE/dplace-data/pull/293)). The Glottolog tree is created using pyglottolog and the python package newick made by Forkel et al, and it's building a tree based on the Language_ID entries in the GB wide file, which is already aggregated for dialects. We don't gain that many data-points honestly by aggregating to languages, but a few and it seems more principled to just lump everything to language so that's what I'm doing. 

## Note on Gray et al 2009-tree
I'm using the all of the posterior trees and the MCCT-tree. I'm using the lexibank/ABVD taxa file rather than D-PLACE/dplace-data. 

Because this takes a signifiant amount of time (there are 4200 trees), I have also stored scripts in this repos which just run over the MCCT tree. These can be found in the directory "analysis_scripts_gray_mcct". They can be used as a demo of sorts for the full analysis.

The full analysis on all the posteriors is in "analysis_scripts_gray_all_posterior".

## Note on tree pruning (general)
For each reconstruction, I'm pruning the relevant tree to only tips which have a value for that feature. I'm not inferring unknown tip states. I could, but I am not. Again, I think principle-wise it'd be a bit too much for traditional linguists to stomach and it shouldn't make much of a difference. For the rest of the results, I'm ignoring trees where more than half tips are missing.

## Functions per method and goal.
I have to use different functions for goals (a) + (b) compared to (c). This is because the conservatism approach I'm using for ML unroots the tree. I'm using Nanggu [nang1262] as outgroup for re-rooting tree.

*Parsimony*

(a) compare with HL and (b) feature rates = `castor::asr_max_parsimony(Nstates = 2, transition_costs = "all_equal")` for the parsimony analysis, 
(c) conservatism = `phangorn::ACCTRAN()`

*ML*

(a) compare with HL and (b) feature rates =  `corHMM::corHMM(  model="ARD", rate.cat = 1, lewis.asc.bias = TRUE, node.states = "marginal",  root.p = "yang")`
(c) conservatism = `phangorn::optim.pml(rearrangement="none")`

*SCM*

