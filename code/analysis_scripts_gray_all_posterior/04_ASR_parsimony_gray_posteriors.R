source("01_requirements.R")
source("fun_custom_parsimony_results_table.R")

#This script reads in all the trees in the posterior of Gray et al 2009. They have already been pruned and tips are set to glottocodes (see 3_get_gray_tree.R). In this script, we apply the function castor::max_parsimony() to each tree and saves the output from that, along with some other handy output regarding tip states etc in a row in a tibble. There is one tibble per tree in the posterior (i.e. 42000) and each tibble has one row per feature, and is saved to an RDS-file. All output is saved to a folder inside output/gray_et_al_2009/parsimony/results_by_tree/. Each folder name is unique to each tree, and all output concerning each tree is saved in the respective folder.

#The setup is as follows
#1) basics are read in
#2) a custom function is defined which runs asr_max_parsimon() and saves some particular output to a list


##Reading in basics

#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, level, classification, Name)

#reading in GB
GB_df_desc <- read_tsv(GB_df_desc_fn, col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

#parameters as a vector to have something to loop over. Each element in this vector also serves as the content of the first column in the output tibble.
GB_parameters <- GB_df_desc  %>% 
  dplyr::select(Feature_ID = ID) %>% 
  as.matrix() %>% 
  as.vector()

#To make things easier for the MP function we are going to use (castor::asr_max_parsimony()) we are going to replace all 1:s with 2:s and all 0:s with 1:s: previously, something seemed to be going awry with the 0:s and this was a hacky, yet, effective solution.
GB_df_all <- read_tsv(GB_binary_fn, col_types = cols()) %>% 
  rename(Glottocode = Language_ID) %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  filter(str_detect(classification, "ocea1241")) %>% #we'll make life easier for the below script as well and tease out only the Oceanic languages as well
  dplyr::select(Glottocode, GB_df_desc$ID) %>% 
  reshape2::melt(id.vars = "Glottocode") %>%
  mutate(value = as.character(value)) %>%  
  mutate(value = ifelse(value == "?", NA, value)) %>% 
  mutate(value = str_replace_all(value, "1", "2")) %>%  
  mutate(value = str_replace_all(value, "0", "1")) %>% 
  mutate(value = as.integer(value)) %>% 
  reshape2::dcast(Glottocode ~ variable)

#reading in gray et all tree, already subsetted to only Oceanic and with tips renamed to glottocodes. If the tip was associated with a dialect which was individually coded in GB, the tip label is the glottocode for that dialect. If not, it has the language-level parent glottocode of that dialect. We'll be dropping tips with missing data feature-wise, i.e. for each feature not before.
gray_trees_fns <- list.files("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", pattern = "*.txt", full.names = T)

##Defining functions

fun_GB_ASR_Parsimony <- function(feature){
#If you'd like to run this script chunk-wise on one feature to understand each step or to debug, you can assign GB020 to the argument "function" by running this line and then run the rest of the lines stepwise as you please.
#feature <- "GB020" 
   
filter_criteria <- paste0("!is.na(", feature, ")")

to_keep <- tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(GB_df_all, by = "Glottocode") %>% 
  filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
  dplyr::select(Glottocode, {{feature}})

gray_tree_pruned <- keep.tip(tree, to_keep$Glottocode)  

#making a named vector for castor__asr_max_parsimony that has the tip labels in the exact same order as the current tree and the associated feature values as values
feature_vec <-  gray_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(GB_df_all, by = "Glottocode") %>% 
  dplyr::select(Glottocode, {{feature}}) %>% 
  tibble::deframe()

#running the ASR
castor_parsimony <- castor::asr_max_parsimony(tip_states = feature_vec, tree = gray_tree_pruned, Nstates = 2, transition_costs = "all_equal")

#setting up things for plotting later
plot_title <- GB_df_desc %>% 
  filter(ID == as.name(feature)) %>% 
  dplyr::select(Grambank_ID_desc) %>% 
  as.matrix() %>% 
  as.vector()

gray_tree_pruned_tip.labels_df <- gray_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df, by = "Glottocode") 

gray_tree_pruned$tip.label <- gray_tree_pruned_tip.labels_df$Name

ntips <- phylobase::nTips(gray_tree_pruned)
ntips_table <- feature_vec %>% table() %>% as.matrix()

cat("I've finished Parsimony ASR with ", fn, " for ", feature, ". \n", sep = "")
output <- list(feature, castor_parsimony, feature_vec, gray_tree_pruned, plot_title, ntips, ntips_table, output_dir)
output
}

###

#looping over all trees in the posterior
#
for(tree_fn in 1:length(gray_trees_fns)){
#for(tree_fn in 1:10){
  
  #tree_fn <- 1
  
  fn_full <- gray_trees_fns[[tree_fn]]
  tree <- read.tree(fn_full)
  fn <-   fn_full %>% basename() %>% str_replace_all(".txt", "")
  #creating a folder for outputting tables
  output_dir <- file.path("output", "gray_et_al_2009", "parsimony", "results_by_tree", fn)
  
  cat(paste0("On ",output_dir, ".\n"))
  
  if(file.exists(  file.path(output_dir, "GB_parsimony_gray_tree.rds")
  )){
    cat(paste0("File already exists, moving on.\n"))
  
  }else{
  if (!dir.exists(output_dir)) { dir.create(output_dir) }
  
  GB_ASR_Parsimony_all_df <- tibble(Feature_ID = GB_parameters,
                                    content = purrr::map(GB_parameters,
                                                         fun_GB_ASR_Parsimony))
  
  saveRDS(GB_ASR_Parsimony_all_df, file = file.path(output_dir, "GB_parsimony_gray_tree.rds"))
  

###Making a summary results table for easy comparison

df_parsimony_gray <- as.data.frame(do.call(rbind,(lapply(GB_ASR_Parsimony_all_df$Feature_ID, 
                                                              fun_extract_tip_counts_parsimony_cost, ASR_tibble = GB_ASR_Parsimony_all_df))))

df_parsimony_gray$ntips <- df_parsimony_gray$`1` + df_parsimony_gray$`2`

df_parsimony_gray %>% 
  rename(`0`= `1`) %>% 
  rename(`1`= `2`) %>% 
  write_csv(file.path(output_dir, "results.csv"))
cat("done summary results table.csv")

cat("ASR with parsimony and Gray et al 2009-tree all done, for ", fn, ".\n" , sep = "")
  }
}
