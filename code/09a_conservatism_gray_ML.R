source("01_requirements.R")

output_dir <- file.path("output", "gray_et_al_2009", "ML", "mcct")
if (!dir.exists(output_dir)) { dir.create(output_dir) }
if (!dir.exists(file.path(output_dir, "tree_plots"))) { dir.create(file.path(output_dir, "tree_plots")) } #dir for tree plot images


#reading in GB
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_")) 

#reading in gray et al tree
gray_tree <- read.newick(file.path("data", "trees", "gray_et_al_tree_pruned_newick_mmct.txt")) 

#making df to inner join GB to
gray_tree_tips_df <- gray_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".")

#pruning GB to only languages in gray et al tree
GB_df_all <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) %>% 
  inner_join(gray_tree_tips_df)

#pruning tree to only tips in GB
gray_tree <- keep.tip(gray_tree, GB_df_all$Language_ID)

#making phydat for phangorn
df_phydat <- GB_df_all %>%
  column_to_rownames("Language_ID") %>% 
  as.matrix() %>% 
  as.phyDat(type="USER", levels=c("0","1"), missing = NA)

start.fit <- pml(gray_tree, df_phydat, bf="empirical", model="f81", )

m <- optim.pml(start.fit, rearrangement="none")

first_split_left <- m$tree[[1]][1,2] 
first_split_right <- m$tree[[1]][1,1]  



tree_rooted<- root_in_edge(m$tree, first_split_left)

ml.tree <- root(m$tree, outgroup = "ayiw1239")



