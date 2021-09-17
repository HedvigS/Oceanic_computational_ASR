source("01_requirements.R")

#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

output_dir <- file.path("output", "gray_et_al_2009", "SCM", "mcct")
if (!dir.exists(output_dir)) { dir.create(output_dir) }
if (!dir.exists(file.path(output_dir, "tree_plots"))) { dir.create(file.path(output_dir, "tree_plots")) } #dir for tree plot images

#reading in gray et all tree, already subsetted to only Oceanic and with tips renamed to glottocodes. If the tip was associated with a dialect which was individually coded in GB, the tip label is the glottocode for that dialect. If not, it has the language-level parent glottocode of that dialect. We'll be dropping tips with missing data feature-wise, i.e. for each feature not before.
gray_tree <- read.newick(file.path("data", "trees", "gray_et_al_tree_pruned_newick_mmct.txt"))

#reading in GB
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_")) 

GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) 

###ASR FUNCTION


fun_GB_ASR_SCM <- function(feature) {
  
  #feature <- "GB024a"
  cat("I've started ASR SCM on ", feature, " with the Gray et al 2009-tree.\n", sep = "")
  
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
  
  if(states == 1) {
    message("All tips for feature ", feature, " are of the same state. We're skipping it, we won't do any ASR or rate estimation for this feature.\n")
    #  beepr::beep(9)
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
    
    output <- list(NA, results_df, NA, NA)
    output
  } else{
    
    result_all_maps <- phytools::make.simmap(tree =   gray_tree_pruned , 
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
      nTips =   gray_tree_pruned $tip.label %>% length(),
      nTips_state_0 =  feature_vec %>% table() %>% as.matrix() %>% .[1,1],
      nTips_state_1 = feature_vec %>% table() %>% as.matrix() %>% .[2,1])
    
    output <- list(result, results_df, result_all_maps, gray_tree_pruned)
    output  
  }
}


GB_ASR_SCM_all <- tibble(Feature_ID = GB_df_desc$ID,
                        content = purrr::map(GB_df_desc$ID,
                                             fun_GB_ASR_SCM ))

#beepr::beep(3)

saveRDS(GB_ASR_SCM_all, file = file.path(output_dir, "GB_SCM_gray_tree_100.rds"))


#GB_ASR_SCM_all <- readRDS(file.path(output_dir, "GB_SCM_gray_tree_100.rds"))
##unraveling the output into a summary table

GB_ASR_SCM_all_split  <- GB_ASR_SCM_all %>% 
  unnest(content) %>% 
  group_by(Feature_ID) %>% 
  mutate(col=seq_along(Feature_ID)) %>%
  spread(key=col, value=content) %>% 
  rename(result_summary = "1", results_df = "2") %>% 
  ungroup()


#making empty df to rbind to

results <- data.frame(
  Variable = NULL,
  LogLikelihood = NULL,
  AICc = NULL,
  pRoot0 = NULL,
  pRoot1 = NULL,
  q01 = NULL,
  q10 = NULL,
  nTips = NULL,
  nTips_state_0 =  NULL,
  nTips_state_1 =  NULL
)


for(row in GB_ASR_SCM_all_split$results_df){
  print(row)
  results <- rbind(results, row)
}

write_csv( results, file = file.path(output_dir,"results_100.csv"))