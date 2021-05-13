#1w:code skirgard$ chmod 755 run_entire_project.sh 
#lingn01w:code skirgard$ ./run_entire_project.sh 

#Step 1
echo first step, installing necessary packages
Rscript 1_requirements.R 
#python3 -m pip install -r 1_requirements.txt

#Step 2
echo second step, fecthing data
Rscript 2_get_glottolog_language_table.R
Rscript 2_get_grambank_data.R 

## 3. prep trees
echo third step, prep trees

#	python3 3_create_tree_bottom_up.py #will create a tree of Oceanic languages where the tips are languoids in grambank
Rscript 3_get_gray_tree.R
	
## 4 run ASR anlsysis -Parsimony
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
#step_4: max_parsimony

echo fourth step, run the max parsimony ancestral state reconstruction analysis
	Rscript 4_ASR_parsimony_glottolog.R
#	Rscript 4_ASR_parsimony_gray.R

## 5 run ASR analysis - Maximum Likelihood
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
#step_5: max_likelihood

echo fifth step, run the max liklihood (marginal) ancestral state reconstruction analysis

#max_likelihood:
	Rscript 5_ASR_ML_glottolog.R
	Rscript 5_ASR_ML_gray.R

## 6 run ASR analysis - SIMMAP

## 7 Compare to HL
echo seventh step, compate to classical historical linguistics

Rscript 7_compare_to_HL.R