#necessary packages
cat("Installing and loading necessary packages. If hiccup occurs, see comment in script.")

source("01_requirements.R")

if(!("rcldf" %in% rownames(installed.packages()))){
p_load("devtools")
install_github("SimonGreenhill/rcldf", dependencies = TRUE)}
library(rcldf)

#if something happens and you can't install glottoTrees for whatever reason, run the next line. It will load a similar function to glottoTrees::keep_as_tip() but homemade by Hedvig and a bit slower than Eric Round's solution in glottoTrees. But! Better than nothing :)
source("fun_keep_as_tip.R")

#reading in and evaluating glottolog-cldf
glottolog_cldf_json <- "../glottolog-cldf/cldf/cldf-metadata.json"

cldf_object <- rcldf::cldf(glottolog_cldf_json)

glottolog_cldf_value_table <- cldf_object$tables$ValueTable

oceanic_tree_full <- glottolog_cldf_value_table %>% 
  filter(Language_ID == "ocea1241") %>% 
  filter(Parameter_ID == "subclassification") %>% 
  dplyr::select("Value") %>% 
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
GB_df <- read.delim("data/GB/GB_wide_binarized.tsv", sep = "\t") %>% 
  dplyr::select(Language_ID) %>% 
  filter(Language_ID != "cent2060") %>% #removing proto-languages
  filter(Language_ID != "east2449") %>% #removing proto-languages
  filter(Language_ID != "poly1242") %>% #removing proto-languages
  filter(Language_ID != "ocea1241") #removing proto-languages

#make df of only things that are languages and oceanic
oceanic_lgs <- read_tsv("data/glottolog_oceanic_languages_df.tsv", show_col_types = F) %>% 
  dplyr::select("Language_ID") %>% 
  as.matrix() %>% 
  as.vector()

#find overlap between glottolog and GB
overlap <- inner_join(GB_df, oceanic_all_labels_df, by = "Language_ID") %>% 
  as.matrix() %>% 
  as.vector()

pruned_tree <-keep_as_tip(oceanic_tree_full, overlap)

pruned_tree  %>% ape::write.tree( "data/trees/glottolog_tree_newick_GB_pruned.txt")

#prune tree to only languages
languages_only_tree <-keep_as_tip(oceanic_tree_full, oceanic_lgs)

languages_only_tree  %>% ape::write.tree("data/trees/glottolog_tree_newick_all_oceanic.txt")