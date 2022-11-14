source("01_requirements.R")

#tree fn vector to loop over
glottolog_tree_fn <- "output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt"
gray_2009_mcct_tree_fn <- "output/processed_data/trees/gray_et_al_tree_pruned_newick_mmct.txt"
gray_posteriors_trees_fns <- list.files("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", pattern = "*.txt", full.names = T)

tree_fns <- c(glottolog_tree_fn, gray_2009_mcct_tree_fn, gray_posteriors_trees_fns)

#reading in GB tables
GB_df <- read_tsv(GB_binary_fn)

GB_df_desc <- read_tsv(GB_df_desc_fn, show_col_types = F) %>% 
  filter(!str_detect(Binary_Multistate, "Multi"))

#feature vector to loop over
features <- GB_df_desc$ID

#df to bind results to in each loop
full_df <- data.frame(Feature = as.character(), 
                      Destimate = as.numeric(), 
                      Pval1 = as.numeric(), 
                      Pval0 = as.numeric(), 
                      n = as.numeric(), 
                      tree = as.character())

for(f in 1:length(features)){

  feature <- features[f]
  
  cat("\n***\nI'm on feature", feature, " which is", f, "out of ", length(features),".\n***\n")
  for(t in tree_fns){
    
    tree <- read.tree(t)
  
    cat("I'm on feature", feature, "and tree", t ,".\n")
    
    df_for_caper <- tree$tip.label %>%
      as.data.frame() %>%
      rename(Language_ID = ".") %>%
      left_join(GB_df, by = "Language_ID") %>% 
      dplyr::select(Language_ID, all_of(feature))
    
    ds <- comparative.data(tree, df_for_caper, names.col=Language_ID)
    
    output <- eval(substitute(phylo.d(data = ds, binvar = this_feature), list(this_feature=as.name(feature))))
    
    spec_df <-   data.frame(Feature = feature, 
                            Destimate = output$DEstimate[[1]], 
                            Pval1 = output$Pval1, 
                            Pval0 = output$Pval0, 
                            n = ds$data %>% nrow(), 
                            tree = basename(t))
    
    full_df <- full_join(full_df, spec_df, by = c("Feature", "Destimate", "Pval1", "Pval0", "n", "tree"))
      }
}

full_df %>% 
  write_tsv("output/HL_comparison/phylo_d_table.tsv", na = "")
