#necessary packages
source("01_requirements.R")

#The next line will load a similar function to glottoTrees::keep_as_tip() but homemade by Hedvig and a bit slower than Eric Round's solution in glottoTrees.
source("fun_keep_as_tip.R")

#reading in and evaluating glottolog-cldf

glottolog_cldf_value_table <- read.delim("output/processed_data/glottolog_language_table_wide_df.tsv", sep = "\t")

oceanic_tree_full <- glottolog_cldf_value_table %>% 
  filter(Language_ID == "ocea1241") %>% 
  dplyr::select("subclassification") %>% 
  as.matrix() %>% 
  as.vector() %>% 
  ape::read.tree(text = .)

#get all labels (nodes and tips)
oceanic_tip_labels <- oceanic_tree_full$tip.label
oceanic_node_labels <- oceanic_tree_full$node.label
oceanic_all_labels_df <- c(oceanic_tip_labels, oceanic_node_labels) %>% 
  as.data.frame() %>%
  rename(Language_ID = ".")

#read in GB
GB_df <- read_tsv(GB_binary_fn) %>% 
  dplyr::select(Language_ID) %>% 
  filter(Language_ID != "cent2060") %>% #removing proto-languages
  filter(Language_ID != "east2449") %>% #removing proto-languages
  filter(Language_ID != "poly1242") %>% #removing proto-languages
  filter(Language_ID != "ocea1241") #removing proto-languages

#make df of only things that are languages and oceanic
oceanic_lgs <- read_tsv("output/processed_data/glottolog_oceanic_languages_df.tsv", show_col_types = F) %>% 
  dplyr::select("Language_ID") %>% 
  as.matrix() %>% 
  as.vector()

#find overlap between glottolog and GB
overlap <- inner_join(GB_df, oceanic_all_labels_df, by = "Language_ID") %>% 
  as.matrix() %>% 
  as.vector()

pruned_tree <-keep_as_tip(oceanic_tree_full, overlap)

pruned_tree <- compute.brlen(pruned_tree, method = 1)

pruned_tree  %>% ape::write.tree( "output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt")

polytomies_n <- pruned_tree$edge %>% 
  as.data.frame() %>% 
  group_by(V1) %>% 
  summarise(n = n()) %>% 
  filter(n > 2) %>% nrow()

splits <- pruned_tree$edge[,1] %>% length()

message("The Oceanic tree (pruned for Grambank matches) has ", splits, " splits. Out of these ",  round(polytomies_n/splits*100, 0), "% are non-binary.")

#prune tree to only languages in oceanic subgroup
oceanic_tree <-keep_as_tip(oceanic_tree_full, oceanic_lgs)

oceanic_tree <- compute.brlen(oceanic_tree, method = 1)



oceanic_tree  %>% ape::write.tree("output/processed_data/trees/glottolog_tree_newick_all_oceanic.txt")