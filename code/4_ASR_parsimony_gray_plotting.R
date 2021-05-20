source("1_requirements.R")
options(tidyverse.quiet = TRUE) 

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
  output_dir <- ACR_object[[8]]
  
  FN_plot <- paste0(feature, ".png")
  FN_ACR <- file.path(output_dir,  FN_plot)
  
  png(file = FN_ACR, width = 8.27, height = 11.69, units = "in", res = 400)
  
  plotTree(ladderize(feature_tree , right = F), offset = 0.8, fsize = fsize)
  
  tiplabels(pie= to.matrix(feature_vec, sort(unique(feature_vec))), piecol=colours, cex = cex_tip, offset = 0.002)
  
  nodelabels(node=1:feature_tree$Nnode+Ntip(feature_tree),
             pie=ACR_parsimony,
             piecol=colours, cex = cex_node)
  
  title(plot_title, cex.main = 1, line = -1)
  
  dev.off()
  cat("I've finished the tree plot for ", feature, " for ", output_dir, "\n", sep = "")
  
}

#looping over the rds-files from 4_ASR_parsimony_Gray, i.e. one rds file for each tree in the posterior

gray_trees_rds_fns <- list.files("output/gray_et_al_2009/parsimony/results_by_tree/", pattern = "*.rds", full.names = T, recursive = T)

for(rds_fn in 1:length(gray_trees_rds_fns)){
  
  #rds_fn <- 1
  
  fn_full <- gray_trees_rds_fns[[rds_fn]]
  
  GB_ASR_Parsimony_all_df <- readRDS(fn_full)
  
  ####PLOTTING TIME
  
  #colors for piecharts
  colours <- c("#8856a7", "#ffffbf")
  ############################################
  
  lapply(X = GB_ASR_Parsimony_all_df$content, ACR_plot)
  }
