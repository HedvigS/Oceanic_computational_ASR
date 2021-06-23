source("01_requirements.R")


#reading in glottolog tree

glottolog <- read_tsv("data/Glottolog_lookup_table_Heti_edition.tsv")

Gray_et_al_tree <- read.nexus("data/trees/gray_et_al2009/summary.trees")
taxa <- read_csv("data/trees/gray_et_al2009/taxa.csv") %>% left_join(glottolog)

Gray_et_al_tree_tip.label_df <- Gray_et_al_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(taxon = ".") %>% 
  full_join(taxa) %>% 
  left_join(glottolog) %>% 
  dplyr::select(glottocode)

#reading in Grambank
GB_df_desc <- read_csv("data/parameters_binary.csv") %>% 
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

GB_df_integers <- read_tsv("data/GB_wide_strict_binarized.tsv") %>% 
  rename(glottocode = Language_ID) %>% 
  dplyr::select(glottocode, GB_df_desc$ID) %>% 
  melt() %>%
  mutate(value = as.character(value)) %>%  
  mutate(value = str_replace_all(value, "1", "2")) %>%  
  mutate(value = str_replace_all(value, "0", "1")) %>% 
  mutate(value = as.integer(value)) %>% 
  dcast(glottocode ~ variable)

GB_df_all_na_prop <- read_tsv("data/GB_wide_strict_binarized.tsv") %>% 
  dplyr::select(glottocode = Language_ID, na_prop)

GB_oceanic_df <- glottolog %>% 
  filter(str_detect(Path, "ocea1241")) %>% 
  inner_join(GB_df_integers) %>% 
  inner_join(GB_df_all_na_prop) %>% 
  filter(!is.na(na_prop)) %>% 
  inner_join(Gray_et_al_tree_tip.label_df) 

GB_oceanic_df_nanggu <- GB_oceanic_df   %>% 
  filter(Name_stripped_no_spaces == "Nanggu") %>% 
  dplyr::select(GB_df_desc$ID) %>% 
  melt() %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(Feature_ID = variable)

###Glottolog parsimony
GB_ACR_all_parsimony <- readRDS("output/ASR/gray_et_al_2009/parsimony/GB_parsimony_gray_tree.rds") %>% 
  inner_join(GB_oceanic_df_nanggu)

asr_parsimony_br_len <- function(ASR_object){

#ASR_object <- GB_ACR_all_parsimony$content[[18]]

feature <-  ASR_object[[1]]
asr_object <-  ASR_object[[2]]
feat_vector <- ASR_object[[3]] %>% as.factor()
tree <-  ASR_object[[4]]

tree <- root(tree, outgroup = "Nanggu")

feat_vector_named <- as.character(feat_vector)
names(feat_vector_named) <- names(feat_vector)
tree$tip.label <- names(feat_vector)

phydat_data <-  as.phyDat(feat_vector_named, type = "USER", levels = c("1", "2") , ambiguity = NA)

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
  write_tsv("output/ASR/conservatism_parsimony_gray.tsv")