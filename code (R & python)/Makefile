# Determine this makefile's path.
THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))

.DEFAULT_GOAL := help

## 1. Check and install dependencies if needed.
requirements:
	Rscript requirements.R | tee $@
	python -m pip install -r requirements.txt
	
## 2. import data from grambank-cldf

get_grambank:
	Rscript get_grambank_data.R 
