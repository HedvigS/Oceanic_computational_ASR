#1w:code skirgard$ chmod 755 run_entire_project_mcct.sh 
#lingn01w:code skirgard$ ./run_entire_project_mcct.sh 

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

Rscript analysis_scripts_gray_mcct/3_get_gray_tree_mcct.R #pruning tree and changing tip names to glottocodes. removing duplicates and merges dialects

#to avoid people having to redo the glottolog-tree, i'm asking if the file already exists (I uploaded it to the repos) and if it does it doesn't run the python script.

FILE=/data/trees/glottolog_4.3_tree_newick.txt
if [ -f "$FILE" ]; then
    echo "$FILE exists."
    echo "Glottolog-tree file already exists, won't bother remaking it."
else 
python3 3_create_glottolog_tree_bottom_up.py #	will create a tree based on glottolog phylogeny of Oceanic languages where the tips are languoids in grambank
fi

## 4 run ASR anlsysis -Parsimony

echo fourth step, run the max parsimony ancestral state reconstruction analysis on the glottolog tree and the MCCT tree from Gray et al 2009.

Rscript analysis_scripts_gray_mcct/4_ASR_parsimony_gray_mmct.R
Rscript 4_ASR_parsimony_glottolog.R

## 5 run ASR analysis - Maximum Likelihood
# These scripts do the ASR and deposits the results in an Rdata file and makes a tree-plot for each feature and method and outputs a PNG file.
#step_5: max_likelihood

echo fifth step, run the max liklihood (marginal) ancestral state reconstruction analysis  on the glottolog tree and the MCCT tree from Gray et al 2009.
Rscript analysis_scripts_gray_mcct/5_ASR_ML_gray_mcct
Rscript 5_ASR_ML_glottolog.R

## 6 run ASR analysis - SIMMAP

## 7 Compare to HL
echo seventh step, compate to classical historical linguistics


#Rscript 7_compare_to_HL.R