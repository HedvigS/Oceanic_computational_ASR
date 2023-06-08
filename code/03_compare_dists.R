source("01_requirements.R")

#reading in trees and computing patricstic distances for the glottolog and mcct tree
glottolog_tree <-read.tree("output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt")
gray_2009_mcct_tree <- read.tree("output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt")

glottolog_tree_dist_matrix <- adephylo::distTips(glottolog_tree) %>% 
  as.matrix() 

glottolog_tree_dist_matrix[upper.tri(glottolog_tree_dist_matrix, diag = T)] <- NA

glottolog_tree_dist_list <- glottolog_tree_dist_matrix %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  rename(glottolog_dist = value)

gray_mcct_dist_matrix <- adephylo::distTips(gray_2009_mcct_tree) %>% 
  as.matrix()

gray_mcct_dist_matrix[upper.tri(gray_mcct_dist_matrix, diag = T)] <- NA

gray_mcct_dist_list <- gray_mcct_dist_matrix %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  rename(gray_mcct_dist = value)

#loop over the posterios trees to get dists
gray_posteriors_trees_fns <- list.files("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", pattern = "*.txt", full.names = T)

df <- data.frame(Var1 = as.character(),
                 Var2 = as.character(),
                 value = as.numeric())

for(fn in gray_posteriors_trees_fns){
  cat(paste0("I'm on tree ", fn, ".\n"))
#  fn <- gray_posteriors_trees_fns[1]
  tree <- read.tree(fn)
  
  dist_matrix <- adephylo::distTips(tree) %>% 
    as.matrix() 
  
 dist_matrix[upper.tri(dist_matrix, diag = T)] <- NA
   
df <- dist_matrix %>% 
  reshape2::melt() %>% 
  filter(!is.na(value)) %>% 
  full_join(df, by = c("Var1", "Var2", "value"))    
}

gray_posterios_dists_list <- df %>% 
  group_by(Var1, Var2) %>% 
  summarise(gray_posteriors_dist = mean(value), .groups = "drop")

#computing GB dists 
GB <- read_tsv("output/GB_wide/GB_wide_binarized.tsv") %>%
  dplyr::select(-na_prop) %>% 
  column_to_rownames("Language_ID")

gb_dist_list <- cluster::daisy(GB, metric = "gower", warnBin = F)  %>% 
  as.matrix() %>% 
  reshape2::melt() %>% 
  rename(GB_dist = value)

joined <-glottolog_tree_dist_list %>% 
  full_join(gray_mcct_dist_list, by = c("Var1", "Var2")) %>% 
  full_join(gray_posterios_dists_list, by = c("Var1", "Var2") ) %>% 
  full_join(gb_dist_list, by = c("Var1", "Var2"))

png(filename = "../tex/illustrations/plots_from_R/SPLOM_tree_dists.png", width = 20, height = 20, units = "cm", res = 400)

joined %>% 
  rename(`Glottolog\ntree tip dist` = glottolog_dist, `Gray et al (2009)\ntree tip dist\nMCCT` = gray_mcct_dist, `Gray et al (2009)\ntree tip dist\n100 posterior trees` = gray_posteriors_dist , `Grambank\ndist` = GB_dist) %>% 
  dplyr::select(-Var1, -Var2) %>% 
  psych::pairs.panels(
  method = "pearson", # correlation method
  hist.col = "#a3afd1",# "#a9d1a3","",""),
  density = TRUE,  # show density plots
  ellipses = F, # show correlation ellipses
  cex.labels= 1.2,
  #           smoother= T,
  cor=T,
  lm=T,
  ci = T, cex.cor = 0.8,stars = T)

x <- dev.off()
