source("01_requirements.R")

#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

#reading in gray et all tree, already subsetted to only Oceanic and with tips renamed to glottocodes. If the tip was associated with a dialect which was individually coded in GB, the tip label is the glottocode for that dialect. If not, it has the language-level parent glottocode of that dialect. We'll be dropping tips with missing data feature-wise, i.e. for each feature not before.
gray_tree <- read.newick(file.path("data", "trees", "gray_et_al_tree_pruned_newick.txt"))
source("1_requirements.R")

#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

#reading in GB
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_")) 

GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) 

###ASR FUNCTION

fun_GB_ASR_SCM <- function(feature) {
  
  #feature <- "GB109"
  cat("I've started ASR SCM on ", feature, " with the gray et al 2009-tree.\n", sep = "")
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
  to_keep <- gray_tree$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
    dplyr::select(Language_ID, {{feature}})
  
  gray_tree_pruned <- keep.tip(gray_tree, to_keep$Language_ID)  
  
  feature_vec <-  gray_tree_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    dplyr::select(Language_ID, {{feature}}) %>% 
    tibble::deframe()
  
  states <-   feature_vec %>% table() %>% length()
  
  if(states >1) {
    
    #replacing branch lengths of length 0 with a teeny-tiny length based on the max heigh of the tree
    gray_tree_pruned $edge.length[gray_tree_pruned $edge.length==0] <- max(nodeHeights(gray_tree_pruned ))*1e-6
    
    
    result <- phytools::make.simmap(tree = gray_tree_pruned, 
                                    x = feature_vec, 
                                    model = "ARD", 
                                    pi = "estimated", 
                                    method = "optim")
    
    
    results_df <- data.frame(
      Feature_ID = feature,
      LogLikelihood = result$logL %>% as.vector(),
      #    AICc = NA,
      #    pRoot0 = NA,
      #    pRoot1 = NA,
      q01 = result$Q[2, 1],
      q10 = result$Q[1, 2],
      nTips = result$Nnode,
      nTips_state_0 =  feature_vec %>% table() %>% as.matrix() %>% .[1,1],
      nTips_state_1 = feature_vec %>% table() %>% as.matrix() %>% .[2,1])
    
    output <- list(result, results_df)
    output  
    
  }else{
    message("All tips for feature ", feature, " are of the same state. We're skipping it, we won't do any ASR or rate estimation for this feature.\n")
    results_df <- data.frame(
      Feature_ID = feature,
      LogLikelihood = NA,
      #    AICc = NA,
      #    pRoot0 = NA,
      #    pRoot1 = NA,
      q01 = NA,
      q10 = NA,
      nTips = NA,
      nTips_state_0 =  NA,
      nTips_state_1 = NA)
    
    output <- list(NA, results_df)
    output  
    
    
  }
}

GB_ASR_SCM_all <- tibble(Feature_ID = GB_df_desc$ID,
                         content = purrr::map(GB_df_desc$ID,
                                              fun_GB_ASR_SCM))

#beepr::beep(3)

saveRDS(GB_ASR_SCM_all, "output/gray_et_al_2009/SCM/GB_SCM_gray_tree.rds")
#GB_ASR_SCM_all <- readRDS("output/gray_et_al_2009/SCM/GB_SCM_gray_tree.rds")

