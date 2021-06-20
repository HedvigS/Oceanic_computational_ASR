source("requirements.R")
options(tidyverse.quiet = TRUE)

glottolog <- read_tsv("../../Glottolog_look_up_table/Glottolog_lookup_table_Hedvig_output/Glottolog_lookup_table_Heti_edition.tsv")

GB_ACR_all_ML_glottolog <- read_csv("output/ASR/glottolog_tree_binary/raydisc/results.csv")

results_FN <- list.files(path = "output/ASR/glottolog_tree_binary/raydisc/", pattern = ".rds", full.names = T)

GB_ACR_all_ML_glottolog$content <- lapply(results_FN, readRDS)

fun_get_ML_branch_lengths <- function(Feature, ASR_tibble) {
  
  #Feature <- "GB038"  
  #ASR_tibble <- GB_ACR_all_ML_glottolog
  print(Feature)
  
  asr_object_row <- ASR_tibble %>% 
    filter(Feature_ID == Feature) 
  
  asr_object <-  asr_object_row$content[[1]]
  
  tree <- asr_object$phy

  sites <- asr_object$data[,2] %>% str_replace_all("0&1", "?") %>% as.matrix()
  
  rownames(sites) <- asr_object$data[,1]
  
  # convert it to a stupid phydat. Change “levels” argument if you have 2 or 3 or 4 present. Make “missing” your missing state (NOT NA)
  pd <- phyDat(as.matrix(sites), type="USER", levels=c("0","1"), missing = "?")
  
  # set up base ml fit.
  start.fit <- pml(tree, pd, bf="empirical", model="f81")
  
  m <- optim.pml(start.fit, rearrangement="none")
  
  #ml.tree <- midpoint(m$tree)  

ml.tree <- root(m$tree, outgroup = "nang1262")
  
dists_ML_dist <- distRoot(ml.tree) %>% 
    as.data.frame() %>% 
    rename(dist = 1) %>% 
    rownames_to_column("glottocode") %>% 
    mutate(Feature_ID = Feature)

dists_nNodes <- distRoot(ml.tree, method = "nNodes") %>% 
  as.data.frame() %>% 
  rename(nNodes_dist = 1) %>% 
  rownames_to_column("glottocode")

dists <- dists_ML_dist %>% full_join(dists_nNodes)
  
  dists
}


Features_to_run_ML_br_len_over <- GB_ACR_all_ML_glottolog %>% 
  filter(Feature_ID != "GB110") %>% 
  filter(Feature_ID != "GB149") %>% 
  filter(Feature_ID != "GB336") %>% 
  filter(Feature_ID != "GB315")

dist_root_rbind_df <- as.data.frame(do.call(rbind,(lapply(Features_to_run_ML_br_len_over$Feature_ID, fun_get_ML_branch_lengths , ASR_tibble = GB_ACR_all_ML_glottolog))))

#writing it to a full tsv
dist_root_rbind_df %>%   write_tsv("output/ASR/conservatism_ML_glottolog_all.tsv")
dist_root_rbind_df <-   read_tsv("output/ASR/conservatism_ML_glottolog_all.tsv")

#removing features for which the language had missing value and this was replaced by "1&0"

GB_long <-  read_tsv("data/GB_wide_strict_binarized.tsv") %>% 
  rename(glottocode = Language_ID) %>% 
  dplyr::select(-na_prop) %>% 
  melt() %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(-value)

dist_root_rbind_df %>% 
  inner_join(GB_long) %>% 
  group_by(glottocode) %>% 
  summarise(mean_dist = mean(dist, na.rm = T), nNodes_mean = mean(nNodes_dist, na.rm = T)) %>% 
  write_tsv("output/ASR/conservatism_ML_glottolog.tsv")

