source("01_requirements.R")

#reading in glottolog language table (for names of tips)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Name)

output_dir <- file.path("output", "gray_et_al_2009", "ML", "mcct")
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


fun_GB_ASR_ML <- function(feature) {

#feature <- "GB109"
cat("I've started ASR ML on ", feature, " with the Gray et al 2009-tree.\n", sep = "")
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
to_keep <- gray_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(GB_df_all, by = "Language_ID") %>% 
  filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
  dplyr::select(Language_ID, {{feature}})

gray_tree_pruned <- keep.tip(gray_tree, to_keep$Language_ID)  

#gray_tree_pruned <- ape::multi2di(gray_tree_pruned) #resolve polytomies to binary splits. This should not have a great effect on the gray et al tree, but due to the pruning it's still worth doing.
#gray_tree_pruned$edge.length[gray_tree_pruned$edge.length==0]<-max(nodeHeights(gray_tree_pruned))*1e-6 #if there are any branch lengths which as 0, make them not zero but a very small value

feature_df <-  gray_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(GB_df_all, by = "Language_ID") %>% 
  dplyr::select(Language_ID, {{feature}}) 

states <- feature_df[,2]  %>% table() %>% length()

if(states == 1) {
  message("All tips for feature ", feature, " are of the same state. We're skipping it, we won't do any ASR or rate estimation for this feature.\n")
#  beepr::beep(9)
  
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
      phy = gray_tree_pruned , 
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
  nTips = phylobase::nTips(gray_tree_pruned), 
  nTips_state_0 =  feature_df[,2]  %>% table() %>% as.matrix() %>% .[1,1] %>% as.vector(),
  nTips_state_1 =  feature_df[,2]  %>% table() %>% as.matrix() %>% .[2,1] %>% as.vector()
)

corHMM::plotRECON(gray_tree_pruned, corHMM_result_direct$states, font=1,
                  use.edge.length = TRUE,
                  piecolors=c("#8856a7", "#ffffbf"),
                  title = sprintf(
                    "Feature: %s (lh=%0.3f, p(root)=%0.2f, %0.2f)",
                    feature, corHMM_result_direct$loglik,
                    corHMM_result_direct$states[1, 1], corHMM_result_direct$states[1, 2]
                  ),
                  file = file.path(output_dir, "tree_plots", paste0("ML_gray_-", feature, ".pdf")),
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

saveRDS(GB_ASR_ML_all, file = file.path(output_dir, "GB_ML_gray_tree.rds"))

##unraveling the output into a summary table

GB_ASR_ML_all_split  <- GB_ASR_ML_all %>% 
  unnest(content) %>% 
  group_by(Feature_ID) %>% 
  mutate(col=seq_along(Feature_ID)) %>%
  spread(key=col, value=content) %>% 
  rename(corHMM_result_direct = "1", results_df = "2") %>% 
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


for(row in GB_ASR_ML_all_split$results_df){
  print(row)
results <- rbind(results, row)
  }

write_csv( results, file = file.path(output_dir,"results.csv"))