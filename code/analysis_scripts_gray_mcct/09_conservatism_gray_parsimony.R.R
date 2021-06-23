source("01_requirements.R")

#reading in gray et all tree, already subsetted to only Oceanic and with tips renamed to glottocodes. If the tip was associated with a dialect which was invidually coded in GB, the tip label is the glottocode for that dialect. If not, it has the language-level parent glottocode of that dialect. We'll be dropping tips with missing data feature-wise, i.e. for each feature not before.
gray_tree <- read.newick(file.path("data", "trees", "gray_et_al_tree_pruned_newick_mmct.txt"))

#reading in Grambank
#reading in Grambank
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

GB_oceanic_df <- read_tsv("data/GB/GB_wide_binarised.tsv") %>%  
  dplyr::select(glottocode = Language_ID, GB_df_desc$ID) %>% 
  reshape2::melt(id.vars = "glottocode") %>%
  mutate(value = as.character(value)) %>%  
  mutate(value = str_replace_all(value, "1", "2")) %>%  
  mutate(value = str_replace_all(value, "0", "1")) %>% 
  mutate(value = as.integer(value)) %>% 
  reshape2::dcast(glottocode ~ variable)

#making list of all features that have at least one of the three lgs we'll use for outgrouping
GB_feats_has_outgroup <- GB_oceanic_df   %>% 
  filter(glottocode == "nang1262"|   glottocode == "ayiw1239"|   glottocode == "natu1246") %>% 
  dplyr::select(glottocode, GB_df_desc$ID) %>% 
  reshape2::melt(id.vars = "glottocode") %>% 
  filter(!is.na(value)) %>% 
  distinct(variable) %>% 
  rename(Feature_ID = variable)

###Glottolog parsimony
GB_ACR_all_parsimony <- readRDS("output/gray_et_al_2009/parsimony/mcct/GB_parsimony_gray_tree.rds") %>% 
  inner_join(GB_feats_has_outgroup)

asr_parsimony_br_len <- function(ASR_object){

#ASR_object <- GB_ACR_all_parsimony$content[[18]]

feature <-  ASR_object[[1]]
asr_object <-  ASR_object[[2]]
feat_vector <- ASR_object[[3]] %>% as.factor()
tree <-  ASR_object[[4]] 

#rerooting  
if("nang1262" %in% tree$tip.label) {
  tree <- root(tree, outgroup = "nang1262")
} else{if("ayiw1239" %in% tree$tip.label){
  tree <- root(tree, outgroup = "ayiw1239")}else{
    if("natu1246" %in% tree$tip.label){
      tree <- root(tree, outgroup = "natu1246")}
  }  }


feat_vector_named <- as.character(feat_vector)
names(feat_vector_named) <- names(feat_vector)
tree$tip.label <- names(feat_vector)

phydat_data <-  as.phyDat(feat_vector_named, type = "USER", levels = c("1", "2") , ambiguity = NA)

tree <- multi2di(tree)

acctran_tree <- acctran(tree = tree, data = phydat_data ) 

df_dist_roots <- distRoot(acctran_tree) %>% as.data.frame()

df_dist_roots_nNodes <- distRoot(acctran_tree, method = "nNodes") %>% as.data.frame() %>% 
  rename(nNodes_dist = ".")

df_dist_roots$glottocode <- names(feat_vector)

df_dist_roots$Grambank_ID <- feature

cat("I'm at ", feature, ".\n")

df_dist_roots %>% 
  cbind(df_dist_roots_nNodes) %>% 
  rename("Cost" = ".")
}

df <- lapply(GB_ACR_all_parsimony$content, asr_parsimony_br_len) %>% bind_rows()

df_summarised <- df %>% 
  group_by(glottocode) %>% 
  summarise(rowmeans = mean(Cost), nNodes_mean = mean(nNodes_dist))

df_summarised  %>% 
  dplyr::select(glottocode, `Mean parsimony cost` = rowmeans, nNodes_mean) %>% 
  write_tsv("output/gray_et_al_2009/parsimony/mcct/conservatism_parsimony_gray.tsv")