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
	Rscript requirements.R | tee $@
	python3 -m pip install -r requirements.txt
		@echo "Installed the necessary packages for python3 and R"
		
## 2. import data

get_glottolog_table: 
	Rscript get_glottolog_language_table.R
	
get_grambank:
	Rscript get_grambank_data.R 
	@echo "Grambank cldf data made wide."

## 3. prep trees
	
glottolog_tree:
	python3 create_tree_bottom_up.py #will create a tree of Oceanic languages where the tips are languoids in grambank
	@echo "Extracted Oceanic subgroup from glottolog tree and pruned so that all tips are languoids in Grambank."
	
gray_et_al_2009_tree:
	Rscript get_gray_tree.R
	
## 4 run ASR anlsysis
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
max_parsimony:
	Rscript ASR_glottolog_parsimony.R
	Rscript ASR_gray_parsimony.R

max_likelihood:


	
