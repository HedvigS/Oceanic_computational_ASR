source("01_requirements.R")

full_df <- read_tsv("output/HL_comparison/phylo_d_table.tsv")

#reading in GB tables
GB_df <- read_tsv(GB_binary_fn)

GB_df_desc <- read_tsv(GB_df_desc_fn, show_col_types = F) %>% 
  filter(!str_detect(Binary_Multistate, "Multi"))

tree <- read.tree("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/gray_et_al_2009_posterior_tree_pruned_17.txt")

feature <- "GB327"

df_for_caper <- tree$tip.label %>%
  as.data.frame() %>%
  rename(Language_ID = ".") %>%
  left_join(GB_df, by = "Language_ID") %>% 
  dplyr::select(Language_ID, all_of(feature))

ds <- comparative.data(tree, df_for_caper, names.col=Language_ID)

output <- eval(substitute(phylo.d(data = ds, binvar = this_feature), list(this_feature=as.name(feature))))


output$DEstimate

