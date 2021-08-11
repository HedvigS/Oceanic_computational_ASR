source("01_requirements.R")

#This script reads in the glottolog 4.3 tree, as prepped by create_tree_bottom_up.py, and GB data, as prepped by get_grambank_data.R, adds the information about GB stats to the tips and runs the function castor::asr_max_parsimony() feature-wise. The tree is pruned to only tips with data for the specific feature.

#reading in glottolog tree
Glottolog_tree_full <- read.tree("data/trees/glottolog_tree_newick_GB_pruned.txt")

#reading in glottolog language table (to be used for Names)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Language_level_ID, level, classification, Name)

#reading in GB
GB_df_desc <-  data.table::fread("data/GB/parameters_binary.tsv" ,
                    encoding = 'UTF-8', 
                    quote = "\"", 
                    fill = T, 
                    header = TRUE, 
                    sep = "\t") %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

#To make things easier for the MP function we are going to use (castor::asr_max_parsimony()) we are going to replace all 1:s with 2:s and all 0:s with 1:s: previously, something seemed to be going awry with the 0:s and this was a hacky, yet, effective solution.
GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) %>% 
  rename(Glottocode = Language_ID) %>% 
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
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
  to_keep <- Glottolog_tree_full$tip.label %>% 
    as.data.frame() %>% 
    rename(Glottocode = ".") %>% 
    left_join(GB_df_all, by = "Glottocode") %>% 
    filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
    group_by(Glottocode) %>% 
    sample_n(1) %>% #removing all duplicate tips. This is done randomly for each iteration, i.e. everytime the function is run on each feature.
    dplyr::select(Glottocode, {{feature}})
  
  Glottolog_tree_full_pruned <- keep.tip(Glottolog_tree_full, to_keep$Glottocode)  
    
#  Glottolog_tree_full_pruned <- ape::multi2di(Glottolog_tree_full_pruned) #resolve polytomies to binary splits. This should not have a great effect on the gray et al tree, but due to the pruning it's still worth doing.

 # Glottolog_tree_full_pruned <- compute.brlen(  Glottolog_tree_full_pruned , method = 1) #making all branch lenghts one
  
  #making a named vector for castor__asr_max_parsimony that has the tip labels in the exact same order as the current tree and the assocaited feature values as values

  feature_vec <-  Glottolog_tree_full_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Glottocode = ".") %>% 
    left_join(GB_df_all, by = "Glottocode") %>% 
    dplyr::select(Glottocode, {{feature}}) %>% 
    tibble::deframe()
  
  #running the ASR
  castor_parsimony <- castor::asr_max_parsimony(tip_states = feature_vec, tree = Glottolog_tree_full_pruned, Nstates = 2, transition_costs = "all_equal")
  
  #setting up things for plotting later
  plot_title <- GB_df_desc %>% 
    filter(ID == as.name(feature)) %>% 
    dplyr::select(Grambank_ID_desc) %>% 
    as.matrix() %>% 
    as.vector()
  
  Glottolog_tree_full_pruned_tip.labels_df <- Glottolog_tree_full_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Glottocode = ".") %>% 
    left_join(glottolog_df, by = "Glottocode") 
  
  Glottolog_tree_full_pruned$tip.label <- Glottolog_tree_full_pruned_tip.labels_df$Name
  
  ntips <- phylobase::nTips(Glottolog_tree_full_pruned)
  ntips_table <- feature_vec %>% table() %>% as.matrix()
  
  cat("I've finished Parsimony ASR with glottolog-tree for  ", feature, ". \n", sep = "")
  output <- list(feature, castor_parsimony, feature_vec, Glottolog_tree_full_pruned, plot_title, ntips, ntips_table)
  output
}

GB_parameters <- GB_df_desc  %>% 
  dplyr::select(Feature_ID = ID) %>% 
  as.matrix() %>% 
  as.vector()

GB_ASR_Parsimony_all_df <- tibble(Feature_ID = GB_parameters,
                                   content = purrr::map(GB_parameters,
                                                        fun_GB_ASR_Parsimony))

saveRDS(GB_ASR_Parsimony_all_df, "output/glottolog_tree_binary/parsimony/GB_parsimony_Glottolog_tree_full.rds")

####PLOTTING TIME

#colors for piecharts
colours <- c("#8856a7", "#ffffbf")
############################################

ACR_plot <- function(ACR_object, fsize = 0.65, cex_tip = 0.13, cex_node = 0.2){
  
  #If you want to step through this function chunkwise, uncomment these lines and run line by line
  #  ACR_object <- GB_ASR_Parsimony_all_df$content[[1]]
  #   fsize = 0.35
  #    cex_tip = 0.13 
  #    cex_node = 0.2
  
  feature <- ACR_object[[1]]
  ACR_parsimony <- ACR_object[[2]][[2]]
  feature_vec <- ACR_object[[3]]
  feature_tree <- ACR_object[[4]]
  FN_obj <- ACR_object[[5]]
  plot_title <- str_replace_all(FN_obj, "_", " ")
  
  FN_ACR <- paste0("output/glottolog_tree_binary/parsimony/tree_plots/parsimony_glottolog_tree_", feature, ".png")
  
  png(file = FN_ACR, width = 8.27, height = 11.69, units = "in", res = 400)
  
  plotTree(ladderize(feature_tree , right = F), offset = 0.8, fsize = fsize)
  
  tiplabels(pie= to.matrix(feature_vec, sort(unique(feature_vec))), piecol=colours, cex = cex_tip, offset = 0.002)
  
  nodelabels(node=1:feature_tree$Nnode+Ntip(feature_tree),
             pie=ACR_parsimony,
             piecol=colours, cex = cex_node)
  
  title(plot_title, cex.main = 1, line = -1)

  dev.off()
  
  cat("I've finished the tree plot for ", feature, ", given glottolog-tree. \n", sep = "")
  
}

lapply(X = GB_ASR_Parsimony_all_df$content, ACR_plot)


###Making a summary results table for easy comparison

source("fun_custom_parsimony_results_table.R")

df_parsimony_glottolog <- as.data.frame(do.call(rbind,(lapply(GB_ASR_Parsimony_all_df$Feature_ID, 
                                                              fun_extract_tip_counts_parsimony_cost, ASR_tibble = GB_ASR_Parsimony_all_df))))


df_parsimony_glottolog$ntips <- df_parsimony_glottolog$`1` + df_parsimony_glottolog$`2`

df_parsimony_glottolog %>% 
  rename(`0`= `1`) %>% 
  rename(`1`= `2`) %>% 
  write_csv("output/glottolog_tree_binary/parsimony/results.csv")

cat("ASR with parsimony and Glottolog-tree all done.")


