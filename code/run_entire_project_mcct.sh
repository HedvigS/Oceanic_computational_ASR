#!/bin/bash
#SBATCH --cpus-per-task 15
#SBATCH --mem 20G
#SBATCH -J Hedvig_Oceanic_ASR_mcct

##1w:code skirgard$ chmod 755 run_entire_project_mcct.sh 
##lingn01w:code skirgard$ ./run_entire_project_mcct.sh 

#Step 1
echo first step, installing necessary packages
Rscript 01_requirements.R 

#Step 2
echo second step, fecthing data
FILE=data/glottolog_language_table_wide_df.tsv
if [ -f "$FILE" ]; then
    echo "$FILE exists."
    echo "Glottolog-cldf table file already exists, won't bother remaking it."
else 
Rscript 02_get_glottolog_language_table.R
fi

FILE=data/GB/GB_wide_binarised.tsv
if [ -f "$FILE" ]; then
    echo "$FILE exists."
    echo "Grambank cldf wide table already exists, won't bother remaking it."
else 
Rscript 02_get_grambank_data.R 
fi

Rscript 02_massage_HL_findings_sheets.R

## 3. prep trees
echo third step, prep trees

Rscript analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R #pruning tree and changing tip names to glottocodes. removing duplicates and merges dialects

Rscript 02_prune_glottolog_tree.R

#visualise coverage
Rscript 03_coverage_viz.R

## 4 run ASR anlsysis -Parsimony

echo fourth step, run the max parsimony ancestral state reconstruction analysis on the glottolog tree and the MCCT tree from Gray et al 2009.

Rscript analysis_scripts_gray_mcct/04_ASR_parsimony_gray_mmct.R
Rscript 04_ASR_parsimony_glottolog.R

## 5 run ASR analysis - Maximum Likelihood
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
#step_5: max_likelihood

echo "fifth step, run the max liklihood (marginal) ancestral state reconstruction analysis  on the glottolog tree and the MCCT tree from Gray et al 2009."
Rscript analysis_scripts_gray_mcct/05_ASR_ML_gray_mcct.R
Rscript 05_ASR_ML_glottolog.R

## 6 run ASR analysis - SIMMAP
echo "fifth step, run the SCM reconstruction analysis  on the glottolog tree and the MCCT tree from Gray et al 2009."
#Rscript analysis_scripts_gray_mcct/06_ASR_SCM_gray_mcct.R
#Rscript 06_ASR_SCM_glottolog.R

## 7 get all the states for 4 proto-languages
echo "seventh step, get ancestral states"

echo "Glottolog tree"
Rscript 07a_get_ancestral_states_parsimony_glottolog.R
Rscript 07b_get_ancestral_states_ML_glottolog.R
#Rscript 07c_get_ancestral_states_SCM_glottolog.R

echo "Gray et al tree (MCCT)"
Rscript analysis_scripts_gray_mcct/07a_get_ancestral_states_parsimony_mcct.R
Rscript analysis_scripts_gray_mcct/07b_get_ancestral_states_ML_mcct.R
#Rscrpt 07c_get_ancestral_states_SCM_mcct.R


echo "I'm all finished with the MCCT analysis!"
#echo "zipping up the posterior results"
#tar czfv parsimony_posteriors.tar.gz output/gray_et_al_2009/parsimony/results_by_tree
#tar czfv ML_posteriors.tar.gz output/gray_et_al_2009/ML/results_by_tree

## 8 compare to HL

#echo eigth step, compate to classical historical linguistics

#Rscript analysis_scripts_gray_mcct/08_compare_to_HL.R