source("01_requirements.R")

#fetching the gray et al 2009 tree from dplace's github repos

config_json <- jsonlite::read_json("config.json")

dplace_github_repos_fn <- config_json$data_sources$d_place_gray_et_al_2009_tree$location

Gray_et_al_tree_fn <- paste0(dplace_github_repos_fn, "/phylogenies/gray_et_al2009/original/a400-m1pcv-time.mcct.trees.gz")

Gray_et_al_tree_taxon_fn <- paste0(dplace_github_repos_fn,"phylogenies/gray_et_al2009/taxa.csv")

Gray_et_al_tree <- read.nexus(Gray_et_al_tree_fn)

if(!is.binary(Gray_et_al_tree) & !is.ultrametric(Gray_et_al_tree)){
  message("The Gray et al 2009 summary tree is not fully ultrametric or binary. However, the tree will not be binraised or made ultrametric as this would distort the branch lengths in an inappropriate way.")
}

#reading in taxa data
taxa <- read_csv(Gray_et_al_tree_taxon_fn, col_types = cols()) %>% 
  rename(Glottocode = glottocode) #to conform to what glottolog does elsewhere 

#reading in grambank data
grambank_df <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/GB_wide_binarized.tsv", col_types = cols()) %>% 
  dplyr::select(Glottocode = Language_ID) %>% 
  mutate(in_GB = "Yes") 

#reading in glottolog language table (to be used for aggregating to Language_level_ID)
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Language_level_ID, level, classification) 

##remove duplicates manually
# For each set of tips which have the same glottocode or are dialects of the same languagaes (and are in GB and in the Oceanic subbgranch) I've gone through and examined the coding in ABVD. I've removed the tip with less data, or in cases where two have similar amounts removed the one which isn't listed as having been "checked" by anyone (assuming that means less reliable). The list below represent the tips that should be removed within the different dialect clusters.

dup_to_remove <- c("Sisingga", 
                   "Carolinian",
                   "Futuna", 
                   "Aria",
                   "Madara",
                   "Maututu",
                   "Chuukese",
                   "NakanaiBileki_D",
                   "LwepeSantaCruz",
                   "Buma",
                   "NehanHape",
                   "Woleai",
                   "Marshallese", 
                   "FutunaWest", #mystery language with no entries
                   "Baliledo" #can't get a glottocode match
)

tree_removed_dups <- drop.tip(Gray_et_al_tree, tip = dup_to_remove)

#renaming tips in the tree to glottocodes. Keeping dialect glottocodes if they are also represented in grambank
Gray_et_al_tree_tip.label_df <- tree_removed_dups$tip.label %>% 
  as.data.frame() %>% 
  rename(taxon = ".") %>% 
  left_join(taxa, by = "taxon") %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  left_join(grambank_df, by = "Glottocode") %>% 
  mutate(Glottocode = ifelse(is.na(in_GB) & level == "dialect", Language_level_ID, Glottocode)) #superceed the specific glottocode with the language level glottocode if there isn't a dialect match

tree_removed_dups$tip.label <- Gray_et_al_tree_tip.label_df$Glottocode

#dropping tips which aren't in GB at all and non-oceanic languages
tips_to_drop <- tree_removed_dups$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  filter(!str_detect(classification, "ocea1241"))
  
tree_pruned <- drop.tip(tree_removed_dups, tips_to_drop$Glottocode)

tree_pruned$edge.length <- tree_pruned$edge.length + 1e-6 #add a tiny branch length to every branch so that there are no branches with 0 length
ape::write.tree(tree_pruned, file = "output/processed_data/trees/gray_et_al_tree_pruned_newick_mmct.txt")