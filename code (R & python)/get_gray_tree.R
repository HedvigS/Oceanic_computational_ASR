source("requirements.R")

#fetching the gray et al 2009 tree from dplace's github repos

config_json <- jsonlite::read_json("config.json")

dplace_github_repos_fn <- config_json$data_sources$d_place_gray_et_al_2009_tree$location

Gray_et_al_tree_fn <- paste0(dplace_github_repos_fn, "/phylogenies/gray_et_al2009/summary.trees")
Gray_et_al_tree_taxon_fn <- paste0(dplace_github_repos_fn, "/phylogenies/gray_et_al2009/taxa.csv")

Gray_et_al_tree <- read.nexus(Gray_et_al_tree_fn)

if(!is.binary(Gray_et_al_tree) & !is.ultrametric(Gray_et_al_tree)){
  message("The Gray et al 2009 summary tree is not fully ultrametric or binary. The tree will be binarised during the analysis, but the branch lengths won't be modified (other than 0 branch lengths getting a tiny-tiny length).")
}

taxa <- read_csv(Gray_et_al_tree_taxon_fn) %>% 
  rename(Glottocode = glottocode) #to conform to what glottolog does elsewhere

#reading in grambank data
grambank_df <- read_tsv("data/GB/GB_wide_binarised.tsv") %>% 
  dplyr::select(Glottocode = Language_ID) %>% 
  mutate(in_GB = "Yes")

#reading in glottolog language table (to be used for aggregating to Language_level_ID)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv")  %>% 
  dplyr::select(Glottocode, Language_level_ID, level, classification)

#renaming tips in the tree to glottocodes. Keeping dialect glottocodes if they are also represented in grambank
Gray_et_al_tree_tip.label_df <- Gray_et_al_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(taxon = ".") %>% 
  full_join(taxa) %>% 
  left_join(glottolog_df) %>% 
  left_join(grambank_df) %>% 
  mutate(Glottocode = ifelse(is.na(in_GB) & level == "dialect", Language_level_ID, Glottocode))

Gray_et_al_tree$tip.label <- Gray_et_al_tree_tip.label_df$Glottocode

#dropping tips which aren't in GB at all and non-oceanic languages
tips_to_drop <- Gray_et_al_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df) %>% 
  filter(!str_detect(classification, "ocea1241"))

tree_pruned <- drop.tip(Gray_et_al_tree, tips_to_drop$Glottocode)

ape::write.tree(tree_pruned, file = file.path("data", "trees", "gray_et_al_tree_pruned_newick.txt"))