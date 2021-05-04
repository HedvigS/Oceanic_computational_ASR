# Determine this makefile's path.
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

.DEFAULT_GOAL := help

#Necessary requirements beyond what is installed by scripts called here

#R should be installed. I haven't done testing, but it should most likely be at least R 3.0
#python3 should be installed
# Glottolog 4.3 should be downloaded from Zenodo and unzipped inside data/glottolog_zenodo
# URL for glottolog cldf zenodo zip file: https://zenodo.org/record/4061162/files/glottolog/glottolog-v4.3.zip?download=1
# Grambank should exist as a cldf repos on the machine. Currently grambank cldf isn't public so I can't set it to grab from the web right now
# grambank-cldf is expected to live at "../../grambank-cldf/"

## 1. Check and install dependencies if needed.
requirements:
	Rscript 1_requirements.R | tee $@
	python3 -m pip install -r 1_requirements.txt
		@echo "Installed the necessary packages for python3 and R"
		
## 2. get cldf-data
get_glottolog_table: 
	Rscript 2_get_glottolog_language_table.R
	
get_grambank:
	Rscript 2_get_grambank_data.R 
	@echo "Grambank cldf data made wide."

## 3. prep trees
glottolog_tree:
	python3 3_create_tree_bottom_up.py #will create a tree of Oceanic languages where the tips are languoids in grambank
	@echo "Extracted Oceanic subgroup from glottolog tree and pruned so that all tips are languoids in Grambank."
	
gray_et_al_2009_tree:
	Rscript 3_get_gray_tree.R
	
## 4 run ASR anlsysis -Parsimony
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
max_parsimony:
	Rscript 4_ASR_parsimony_glottolog.R
	Rscript 4_ASR_parsimony_gray.R

## 5 run ASR analysis - Maximum Likelihood
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
max_likelihood:
	Rscript 5_ASR_ML_glottolog.R
	Rscript 5_ASR_ML_gray.R

## 6 run ASR analysis - SIMMAP

## 7 Compare to HL
summary_tables:
	Rscript 7_compare_to_HL.R

##  

## 8 Calculate conservatism

