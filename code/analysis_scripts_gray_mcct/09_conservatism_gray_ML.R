source("01_requirements.R")



glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  
GB_ACR_all_ML_gray_summaries <- read_csv("output/gray_et_al_2009/ML/mcct/results.csv")

results_FN <- list.files(path = "output/gray_et_al_2009/ML/mcct/", pattern = ".rds", full.names = T)

GB_ACR_all_ML_gray <- results_FN %>% 
  readRDS() %>% 
  full_join(GB_ACR_all_ML_gray_summaries )


fun_get_ML_branch_lengths <- function(Feature, ASR_tibble) {
  
#Feature <- "GB038"  
#ASR_tibble <- GB_ACR_all_ML_gray
  print(Feature)

asr_object_row <- ASR_tibble %>% 
  filter(Feature_ID == Feature) 

asr_object <-  asr_object_row$content[[1]][[1]]

tree <- asr_object$phy

sites <- asr_object$data[,2] %>% str_replace_all("0&1", "?") %>% as.matrix()

rownames(sites) <- asr_object$data[,1]

# convert it to a stupid phydat. Change “levels” argument if you have 2 or 3 or 4 present. Make “missing” your missing state (NOT NA)
pd <- phyDat(as.matrix(sites), type="USER", levels=c("0","1"), missing = "?")

# set up base ml fit.
start.fit <- pml(tree, pd, bf="empirical", model="f81")

m <- optim.pml(start.fit, rearrangement="none")

if("nang1262" %in% m$tree$tip.label) {
  ml.tree <- root(m$tree, outgroup = "nang1262")
} else{if("ayiw1239" %in% m$tree$tip.label){
  ml.tree <- root(m$tree, outgroup = "ayiw1239")}else{
    if("natu1246" %in% m$tree$tip.label){
      ml.tree <- root(m$tree, outgroup = "natu1246")}else{cat("Tree doesn't have any of the three outgroup lg candidates.\n")}
    
  }
  
}

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


Features_to_run_ML_br_len_over <- GB_ACR_all_ML_gray %>% 
  filter(Feature_ID != "GB046") %>% #too skewed
  filter(Feature_ID != "GB135") %>% #too skewed
  filter(Feature_ID != "GB146") %>% #too skewed
  filter(Feature_ID != "GB187") %>% #too skewed
  filter(Feature_ID != "GB188") %>% #too skewed
  filter(Feature_ID != "GB149") %>% #too skewed
  filter(Feature_ID != "GB167") %>% #too skewed
  filter(Feature_ID != "GB196") %>% #too skewed
  filter(Feature_ID != "GB197") %>% #too skewed
  filter(Feature_ID != "GB315") %>% #too skewed
  filter(Feature_ID != "GB260") %>% #too skewed
  filter(Feature_ID != "GB402") %>% #too skewed
  filter(Feature_ID != "GB204") %>% #not outgroup lgs
  filter(Feature_ID != "GB415") %>% #not outgroup lgs
  filter(Feature_ID != "GB323") %>% #not outgroup lgs
  filter(Feature_ID != "GB205") %>% #not outgroup lgs
  filter(Feature_ID != "GB325") %>% #not outgroup lgs
  filter(Feature_ID != "GB305") %>% #not outgroup lgs
  filter(Feature_ID != "GB296") %>% #not outgroup lgs
  filter(Feature_ID != "GB110") #%>% #too skewed

dist_root_rbind_df <- as.data.frame(do.call(rbind,(lapply(Features_to_run_ML_br_len_over$Feature_ID, fun_get_ML_branch_lengths , ASR_tibble = GB_ACR_all_ML_gray))))

dist_root_rbind_df %>%   write_tsv("output/gray_et_al_2009/ML/mcct/conservatism_ML_gray_all.tsv")

#removing features for which the language had missing value and this was replaced by "1&0"
GB_long <-  read_tsv("data/GB/GB_wide_binarised.tsv") %>% 
  rename(glottocode = Language_ID) %>% 
  reshape2::melt(id.vars = "glottocode") %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(-value)

dist_root_rbind_df %>% 
  inner_join(GB_long) %>% 
  group_by(glottocode) %>% 
  summarise(mean_dist = mean(dist, na.rm = T), nNodes_mean = mean(nNodes_dist, na.rm = T)) %>% 
  write_tsv("output/gray_et_al_2009/ML/mcct/conservatism_ML_gray.tsv")

library(beepr)
beepr::beep(3)
