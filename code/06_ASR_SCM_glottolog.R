source("01_requirements.R")

#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

glottolog_tree <- read.newick(file.path("data", "trees", "glottolog_tree_newick_GB_pruned.txt"))

#reading in GB
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_")) 

GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) 

###ASR FUNCTION

fun_GB_ASR_SCM <- function(feature) {
  
  #feature <- "GB109"
  cat("I've started ASR SCM on ", feature, " with the glottolog-tree.\n", sep = "")
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
  to_keep <- glottolog_tree$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
    dplyr::select(Language_ID, {{feature}})
  
  glottolog_tree_pruned <- keep.tip(glottolog_tree, to_keep$Language_ID)  

#  glottolog_tree_pruned <- ape::multi2di(glottolog_tree_pruned) #resolve polytomies to binary splits. This should not have a great effect on the gray et al tree, but due to the pruning it's still worth doing.
  
 glottolog_tree_pruned <- compute.brlen(  glottolog_tree_pruned , method = 1) #making all branch lengths one

  feature_vec <-  glottolog_tree_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    dplyr::select(Language_ID, {{feature}}) %>% 
    tibble::deframe()

states <-   feature_vec %>% table() %>% length()
    
if(states >1) {

  result_all_maps <- phytools::make.simmap(tree = glottolog_tree_pruned, 
                                  x = feature_vec, 
                                  model = "ARD", 
                                  nsim = 100,
                                  pi = "estimated", 
                                  method = "optim")

result <-   result_all_maps %>% summary()
  
    results_df <- data.frame(
    Feature_ID = feature,
    LogLikelihood = NA,
    q01 = NA,
    q10 = NA,
    nTips = glottolog_tree_pruned$tip.label %>% length(),
    nTips_state_0 =  feature_vec %>% table() %>% as.matrix() %>% .[1,1],
    nTips_state_1 = feature_vec %>% table() %>% as.matrix() %>% .[2,1])
  
output <- list(result , results_df)
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

saveRDS(GB_ASR_SCM_all, "output/glottolog_tree_binary/SCM/GB_SCM_glottolog_tree.rds")
#GB_ASR_SCM_all <- readRDS( "output/glottolog_tree_binary/SCM/GB_SCM_glottolog_tree.rds")
