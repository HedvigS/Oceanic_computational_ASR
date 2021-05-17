source("1_requirements.R")

#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

glottolog_tree <- read.newick(file.path("data", "trees", "glottolog_4.3_tree_newick.txt"))

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
  
  feature_vec <-  glottolog_tree_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    dplyr::select(Language_ID, {{feature}}) %>% 
    tibble::deframe()

states <-   feature_vec %>% table() %>% length()
    
if(states >1) {

  result <- phytools::make.simmap(tree = glottolog_tree_pruned, 
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

beepr::beep(3)

saveRDS(GB_ASR_SCM_all, "output/glottolog_tree_binary/SCM/GB_SCM_glottolog_tree.rds")
#GB_ASR_SCM_all <- readRDS( "output/glottolog_tree_binary/SCM/GB_SCM_glottolog_tree.rds")





