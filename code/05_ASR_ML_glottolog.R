source("01_requirements.R")

#reading in glottolog tree
glottolog_tree <- read.tree("output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt")
glottolog_tree$node.label <- NULL

#reading in glottolog language table (to be used for Names)
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Language_level_ID, level, classification, Name)

#reading in GB
GB_df_desc <-  data.table::fread(GB_df_desc_fn,
                                 encoding = 'UTF-8', 
                                 quote = "\"", 
                                 fill = T, 
                                 header = TRUE, 
                                 sep = "\t") %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

#reading in Grambank
GB_df_all <- read_tsv(GB_binary_fn, col_types = cols()) 

##ASR function
fun_GB_ASR_ML <- function(feature) {
  
  #feature <- "GB109"
  cat("I've started ASR ML on ", feature, " with the glottolog-tree.\n", sep = "")
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
  to_keep <- glottolog_tree$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
    dplyr::select(Language_ID, {{feature}})
  
  tree_pruned <- keep.tip(glottolog_tree, to_keep$Language_ID)  %>% ladderize(right = F)

  tree_pruned <- compute.brlen(tree_pruned, method = 1) #making all branch lenghts one in case pruning made some 0
  
  feature_df <-  tree_pruned$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(GB_df_all, by = "Language_ID") %>% 
    dplyr::select(Language_ID, {{feature}}) 
  
  #making a table for number of 0s and 1s, taking into account when there is only one of them. This will populate the columns for nTips_state_0 and nTips_state_1 later.
  
  #counting the number of 0s and 1s
  x <- feature_df[,2]  %>% table()
  
  #checking if there are both 0s and 1s, or only just one of them
  states <- length(x)
  are_there_zeroes <- "0" %in% dimnames(x)[[1]]
  are_there_ones <- "1" %in% dimnames(x)[[1]]
  
  #making a count table in cases where there are no 0s
  if(are_there_zeroes == F){
    x <- tibble("0 " = 0,
                "1 " = x %>% as.matrix() %>% .[1,1])
  }
  
  #making a count table in cases where there are no 1s
  if(are_there_ones == F){
    x <- tibble("0" = x %>% as.matrix() %>% .[1,1],
                "1" = 0)
  }
  
  #making a count table in cases where there are 0s and 1s
  if(are_there_zeroes == T & are_there_ones == T){
    x <- tibble("0" = x %>% as.matrix() %>% .[1,1] %>% as.vector(),
                "1"= x %>% as.matrix() %>% .[2,1] %>% as.vector())
  }
  
  #make variables to use later
  nTips_state_0 = x$`0`[1]
  nTips_state_1 = x$`1`[1]
  
  if(states == 1 | nTips(tree_pruned) < ntips_half_glottolog) {
    message("All tips for feature ", feature, " are of the same state or there are too few tips. We're skipping it, we won't do any ASR or rate estimation for this feature.\n")

results_df <- data.frame(
      Feature_ID = feature,
      LogLikelihood = NA,
      AICc = NA,
      pRoot0 = NA,
      pRoot1 = NA,
      q01 = NA,
      q10 = NA,
      nTips = nTips(tree_pruned),
      nTips_state_0 =  nTips_state_0,
      nTips_state_1 = nTips_state_1)
    
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
      nTips_state_0 =  nTips_state_0,
      nTips_state_1 =  nTips_state_1
    )
    
  tree_pruned_tip.labels_df <- tree_pruned$tip.label %>% 
      as.data.frame() %>% 
      rename(Glottocode = ".") %>% 
      left_join(glottolog_df, by = "Glottocode") 
    
    tree_pruned$tip.label <- tree_pruned_tip.labels_df$Name
    
    tree_pruned$root.edge <- 0.3
    
png(filename = file.path(OUTPUTDIR_plots , "tree_plots", "glottolog-tree", "ML",paste0( "ML_glottolog_tree_", feature, ".png")), width = 15, height = 22, units = "cm", res = 400)

plotRECON_tweaked(tree_pruned, corHMM_result_direct$states, font=1,
                      use.edge.length = TRUE,
                      piecolors=colours_binary,
                      show.legend = F,
                      no.margin = T,
                      root.edge = T,
                      tip_states = feature_df[,2],
                      text.x = 258, text.pos = 4, text.cex = 1,
                      title = paste0("Gray et al (2009)-mcct, ML: ", feature)
    )

    x <- dev.off()
    cat("Done with ASR ML on ", feature, ".\n", sep = "")
    
    #beepr::beep(2)
    output <- list(corHMM_result_direct, results_df)
    output
  }
}
if(file.exists("output/glottolog-tree/ML/GB_ML_glottolog_tree.rds")){
  cat(paste0("File already exists for Glottolog ML, moving on.\n"))
}else{


GB_ASR_ML_all <- tibble(Feature_ID = GB_df_desc$ID,
                        content = purrr::map(GB_df_desc$ID,
                                             fun_GB_ASR_ML ))
#beepr::beep(3)



saveRDS(GB_ASR_ML_all, "output/glottolog-tree/ML/GB_ML_glottolog_tree.rds")
#GB_ASR_ML_all <- readRDS("output/glottolog-tree/ML/GB_ML_glottolog_tree.rds")

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

write_csv( results, "output/glottolog-tree/ML/results.csv")

}