source("01_requirements.R")

fun_extract_tip_counts_parsimony_cost <- function(ASR_tibble, feature) {
  
  #feature <- "GB111"
  #ASR_tibble <- GB_ACR_all_parsimony_glottolog
  filter_criteria <-   paste0("Feature_ID == \"", feature, "\"")
  
  df_filtered <- ASR_tibble %>% 
    filter(eval(parse(text = filter_criteria)))
  
  cost <-   df_filtered$content[[1]][[2]]$total_cost %>% as.matrix()
  colnames(cost) <- c("Parsimony_cost")
  
  feature <- feature %>% as.matrix()
  colnames(feature) <- "Feature_ID"
  
  tip_counts <- df_filtered$content[[1]][[7]] %>% as.matrix() %>% t() %>% as.data.frame()
  
  if(ncol(tip_counts) == 1){
    tip_counts$`2` <- 0
  } 
  
  matrix <- cost %>% cbind(tip_counts) %>% cbind(feature) %>% as.data.frame()
  matrix
}
