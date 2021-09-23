source("01_requirements.R")


#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

#reading in glottolog tree
tree <- read.tree("data/trees/glottolog_tree_newick_GB_pruned.txt")

#reading in Grambank
GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) 

GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

##ASR function


fun_GB_ASR_ML <- function(feature) {
  
  #feature <- "GB109"
  cat("I've started ASR ML on ", feature, " with the glottolog-tree.\n", sep = "")
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
  to_keep <- tree$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
    dplyr::select(Language_ID, {{feature}})
  
  tree_pruned <- keep.tip(tree, to_keep$Language_ID)  
  
#  tree_pruned<- ape::multi2di(tree_pruned) #resolve polytomies to binary splits. This should not have a great effect on the gray et al tree, but due to the pruning it's still worth doing.
  
  tree_pruned <- compute.brlen(tree_pruned, method = 1) #making all branch lenghts one
  
  feature_df <-  tree_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    dplyr::select(Language_ID, {{feature}}) 
  
  states <- feature_df[,2]  %>% table() %>% length()
  
  if(states == 1) {
    message("All tips for feature ", feature, " are of the same state. We're skipping it, we won't do any ASR or rate estimation for this feature.\n")

results_df <- data.frame(
      Feature_ID = feature,
      LogLikelihood = NA,
      AICc = NA,
      pRoot0 = NA,
      pRoot1 = NA,
      q01 = NA,
      q10 = NA,
      nTips = NA,
      nTips_state_0 =  NA,
      nTips_state_1 = NA)
    
    output <- list("NA", results_df)
    output
  } else{
    
    
    # If I decide to switch back and have unknown tips in, replace ? or missing tips with "0&1" or leave as NA and don't prune
    
    corHMM_result_direct <- corHMM::corHMM(
      phy = tree_pruned , 
      data = feature_df, 
      model="ARD",
      rate.cat = 1,
      lewis.asc.bias = TRUE,
      node.states = "marginal",  # joint, marginal, scaled
      root.p = "yang" 
    )
    
    results_df <- data.frame(
      Feature_ID = feature,
      LogLikelihood = corHMM_result_direct$loglik,
      AICc = corHMM_result_direct$AICc,
      pRoot0 = corHMM_result_direct$states[1, 1],
      pRoot1 = corHMM_result_direct$states[1, 2],
      q01 = corHMM_result_direct$solution[1,][2],
      q10 = corHMM_result_direct$solution[2,][1],
      nTips = phylobase::nTips(tree_pruned), 
      nTips_state_0 =  feature_df[,2]  %>% table() %>% as.matrix() %>% .[1,1] %>% as.vector(),
      nTips_state_1 =  feature_df[,2]  %>% table() %>% as.matrix() %>% .[2,1] %>% as.vector()
    )
    
    corHMM::plotRECON(tree_pruned, corHMM_result_direct$states, font=1,
                      use.edge.length = TRUE,
                      piecolors=c("#8856a7", "#ffffbf"),
                      title = sprintf(
                        "Feature: %s (lh=%0.3f, p(root)=%0.2f, %0.2f)",
                        feature, corHMM_result_direct$loglik,
                        corHMM_result_direct$states[1, 1], corHMM_result_direct$states[1, 2]
                      ),
                      file = sprintf("output/glottolog_tree_binary/ML/tree_plots/ML_glottolog_-%s.pdf", feature),
                      width=8, height=16
    )
    
    cat("Done with ASR ML on ", feature, ".\n", sep = "")
    
    #beepr::beep(2)
    output <- list(corHMM_result_direct, results_df)
    output
  }
}


GB_ASR_ML_all <- tibble(Feature_ID = GB_df_desc$ID,
                        content = purrr::map(GB_df_desc$ID,
                                             fun_GB_ASR_ML ))

#beepr::beep(3)

saveRDS(GB_ASR_ML_all, "output/glottolog_tree_binary/ML/GB_ML_glottolog_tree.rds")
#GB_ASR_ML_all <- readRDS("output/glottolog_tree_binary/ML/GB_ML_glottolog_tree.rds")

##unraveling the output into a summary table

GB_ASR_ML_all_split  <- GB_ASR_ML_all %>%
  unnest(content) %>% 
  group_by(Feature_ID) %>% 
  mutate(col=seq_along(Feature_ID)) %>%
  spread(key=col, value=content) %>% 
  rename(SIMMAP_result = "1", results_df = "2") %>% 
  ungroup()


#making empty df to rbind to

results <- data.frame(
  Feature_ID = NULL,
  LogLikelihood = NULL,
  AICc = NULL,
  pRoot0 = NULL,
  pRoot1 = NULL,
  q01 = NULL,
  q10 = NULL,
  nTips = NULL,
  nTips_state_0 =  NULL,
  nTips_state_1 = NULL
)


for(row in GB_ASR_ML_all_split$results_df){
  print(row)
  results <- rbind(results, row)
}

write_csv( results, "output/glottolog_tree_binary/ML/results.csv")