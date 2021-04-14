source("requirements.R")
options(tidyverse.quiet = TRUE) 

#This script reads in the gray et al 2009 tree, as prepped by get_gray_tree.R, and GB data, as prepped by get_grambank_data.R, adds the information about GB stats to the tips and runs the function castor::asr_max_parsimony() feature-wise. The tree is pruned to only tips with data for the specific feature.

#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, level, classification, Name)

#reading in gray et all tree, already subsetted to only Oceanic and with tips renamed to glottocodes. If the tip was associated with a dialect which was invidually coded in GB, the tip label is the glottocode for that dialect. If not, it has the language-level parent glottocode of that dialect. We'll be dropping tips with missing data feature-wise, i.e. for each feature not before.
gray_tree <- read.newick(file.path("data", "trees", "gray_et_al_tree_pruned_newick.txt"))

#reading in GB
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

#To make things easier for the MP function we are going to use (castor::asr_max_parsimony()) we are going to replace all 1:s with 2:s and all 0:s with 1:s: previously, something seemed to be going awry with the 0:s and this was a hacky, yet, effective solution.
GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) %>% 
  rename(Glottocode = Language_ID) %>% 
  left_join(glottolog_df) %>% 
  filter(str_detect(classification, "ocea1241")) %>% #we'll make life easier for the below script as well and tease out only the Oceanic languages as well
  dplyr::select(Glottocode, GB_df_desc$ID) %>% 
  reshape2::melt(id.vars = "Glottocode") %>%
  mutate(value = as.character(value)) %>%  
  mutate(value = str_replace_all(value, "1", "2")) %>%  
  mutate(value = str_replace_all(value, "0", "1")) %>% 
  mutate(value = as.integer(value) %>% suppressWarnings()) %>% 
  reshape2::dcast(Glottocode ~ variable)

fun_GB_ASR_Parsimony <- function(feature){
#If you'd like to run this script chunk-wise on one feature to understand each step or to debug, you can assign GB020 to the argument "function" by running this line and then run the rest of the lines stepwise as you please.
#feature <- "GB020" 
   
filter_criteria <- lazyeval::interp(~!is.na(x), .values = list(x = as.name(feature)))
  
to_keep <- gray_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(GB_df_all) %>% 
  filter_(filter_criteria) %>% #removing all tips that don't have data for the relevant feature
  group_by(Glottocode) %>% 
  sample_n(1) %>% #removing all duplicate tips. This is done randomly for each iteration, i.e. everytime the function is run on each feature.
  dplyr::select(Glottocode, {{feature}})
  
gray_tree_pruned <- keep.tip(gray_tree, to_keep$Glottocode)  

gray_tree_pruned <- ape::multi2di(gray_tree_pruned) #resolve polytomies to binary splits. This should not have a great effect on the gray et al tree, but due to the pruning it's still worth doing.
gray_tree_pruned$edge.length[gray_tree_pruned$edge.length==0]<-max(nodeHeights(gray_tree_pruned))*1e-6 #if there are any branch lengths which as 0, make them not zero but a very small value

#making a named vector for castor__asr_max_parsimony that has the tip labels in the exact same order as the current tree and the assocaited feature values as values
feature_vec <-  gray_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(GB_df_all) %>% 
  dplyr::select(Glottocode, {{feature}}) %>% 
   tibble::deframe()

cat("I've started running castor::asr_max_parsimony() on ", feature, ".\n", sep = "")

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
  left_join(glottolog_df) 

gray_tree_pruned$tip.label <- gray_tree_pruned_tip.labels_df$Name

ntips <- phylobase::nTips(gray_tree_pruned)
ntips_table <- feature_vec %>% table() %>% as.matrix()

cat("I've finished Parsimony ASR for  ", feature, ". \n", sep = "")
output <- list(feature, castor_parsimony, feature_vec, gray_tree_pruned, plot_title, ntips, ntips_table)
output
}

GB_parameters <- GB_df_desc  %>% 
  dplyr::select(Feature_ID = ID) %>% 
    as.matrix() %>% 
  as.vector()

fun_GB_ASR_Parsimony_all <- tibble(Feature_ID = GB_parameters,
                           content = purrr::map(GB_parameters,
                                                fun_GB_ASR_Parsimony))

saveRDS(fun_GB_ASR_Parsimony_all, "output/gray_et_al_2009/parsimony/GB_parsimony_gray_tree.rds")

####PLOTTING TIME

#colors for piecharts
colours <- c("#8856a7", "#ffffbf")
############################################

ACR_plot <- function(ACR_object, fsize = 0.65, cex_tip = 0.13, cex_node = 0.2){
  
  #If you want to step through this function chunkwise, uncomment these lines and run line by line
#  ACR_object <- fun_GB_ASR_Parsimony_all$content[[1]]
#   fsize = 0.35
#    cex_tip = 0.13 
#    cex_node = 0.2
  
  feature <- ACR_object[[1]]
  ACR_parsimony <- ACR_object[[2]][[2]]
  feature_vec <- ACR_object[[3]]
  feature_tree <- ACR_object[[4]]
  FN_obj <- ACR_object[[5]]
  plot_title <- str_replace_all(FN_obj, "_", " ")
  
  cat("I've started making the parsimony tree plot for ", feature, ". \n", sep = "")
  
  FN_ACR <- paste0("output/gray_et_al_2009/parsimony/tree_plots/parsimony_gray_tree_", feature, ".png")
  
  png(file = FN_ACR, width = 8.27, height = 11.69, units = "in", res = 400)
  
  plotTree(ladderize(feature_tree , right = F), offset = 0.8, fsize = fsize)
  
  tiplabels(pie= to.matrix(feature_vec, sort(unique(feature_vec))), piecol=colours, cex = cex_tip, offset = 0.002)
  
  nodelabels(node=1:feature_tree$Nnode+Ntip(feature_tree),
             pie=ACR_parsimony,
             piecol=colours, cex = cex_node)
  
  title(plot_title, cex.main = 1, line = -1)
  
  dev.off()
  cat("I've finished ", feature, ". \n", sep = "")
  
}

lapply(X = fun_GB_ASR_Parsimony_all$content, ACR_plot)