source("01_requirements.R")

#fetching the gray et al 2009 trees from dplace's github repos, dropping unwanted duplicates and renaming all tips to their respective language level glottocodes

config_json <- jsonlite::read_json("config.json")

dplace_github_repos_fn <- config_json$data_sources$d_place_gray_et_al_2009_tree$location

Gray_et_al_trees_fn <- paste0(dplace_github_repos_fn, "/phylogenies/gray_et_al2009/original/a400-m1pcv-time.trees.gz")

Gray_et_al_tree_taxon_fn <- paste0(dplace_github_repos_fn,"phylogenies/gray_et_al2009/taxa.csv")

Gray_et_al_trees <- read.nexus(Gray_et_al_trees_fn)

Gray_et_al_trees <- sample(Gray_et_al_trees, size=100)

#reading in taxa data
taxa <- read_csv(Gray_et_al_tree_taxon_fn, col_types = cols()) %>% 
  rename(Glottocode = glottocode) #to conform to what glottolog does elsewhere 

#reading in grambank data
grambank_df <- read_tsv(GB_binary_fn, col_types = cols()) %>% 
  dplyr::select(Glottocode = Language_ID) %>% 
  mutate(in_GB = "Yes") 

#reading in glottolog language table (to be used for aggregating to Language_level_ID)
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Language_level_ID, level, classification) 

##remove duplicates manually
# For each set of tips which have the same glottocode or are dialects of the same languages (and are in GB and in the Oceanic subbranch) I've gone through and examined the coding in ABVD. I've removed the tip with less data, or in cases where two have similar amounts removed the one which isn't listed as having been "checked" by anyone (assuming that means less reliable). The list below represent the tips that should be removed within the different dialect clusters.
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


index <- 0
multiPhylo_obj <- ""
class(multiPhylo_obj) <- "multiPhylo"

for(tree in 1:length(Gray_et_al_trees)){
  index <- index +1
#  tree <- 1
  
tree <- Gray_et_al_trees[[tree]]
tree_removed_dups <- drop.tip(tree, tip = dup_to_remove)

#renaming tips in the tree to glottocodes. Keeping dialect glottocodes if they are also represented in grambank
tree_tip.label_df <- tree_removed_dups$tip.label %>% 
  as.data.frame() %>% 
  rename(taxon = ".") %>% 
  left_join(taxa, by = "taxon") %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  left_join(grambank_df, by = "Glottocode") %>% 
  mutate(Glottocode = ifelse(is.na(in_GB) & level == "dialect", Language_level_ID, Glottocode)) #superceed the specific glottocode with the language level glottocode if there isn't a dialect match

tree_removed_dups$tip.label <- tree_tip.label_df$Glottocode

#dropping tips which aren't in GB at all and non-oceanic languages
tips_to_drop <- tree_removed_dups$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  filter(!str_detect(classification, "ocea1241"))

tree_pruned_for_oceanic <- drop.tip(tree_removed_dups, tips_to_drop$Glottocode)

#dropping tips which are not in GB
tips_to_drop <- tree_pruned_for_oceanic$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  anti_join(grambank_df, by = "Glottocode")

tree_pruned <- drop.tip(tree_pruned_for_oceanic, tips_to_drop$Glottocode)

if(!is.rooted(tree_pruned)){
  
  cat(paste0("Resulting pruned tree isn't rooted. Rooting with Nanggu as outgroup.\n"))
  tree_pruned <- ape::root(phy = tree_pruned, outgroup = "nang1262", resolve.root = T)
    }

# do not collapse 0-branches into polytomies, because it results in basal polytomies which breaks analysis
#tree_pruned <- ape::di2multi(tree_pruned)

tree_pruned$edge.length <- tree_pruned$edge.length + 1.1e-4 #add a tiny branch lenght to every branch so that there are no branches with 0 length


tree_fn <- paste0("gray_et_al_2009_posterior_tree_pruned_", index, ".txt")

ape::write.tree(tree_pruned, paste0("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", tree_fn))
cat("I'm done with pruning tree ", index, " in the posterior.\n", sep = "")
multiPhylo_obj <- c(multiPhylo_obj,tree_pruned)
}

multiPhylo_obj[-1] %>% 
  ape::write.tree(file = "output/processed_data/trees/gray_et_al_2009_posterios_pruned_multiPhylo.txt")