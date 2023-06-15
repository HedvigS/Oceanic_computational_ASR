source("01_requirements.R")

HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") 

fns <- list.files("output/HL_comparison/phylo_d/", pattern = "main", full.names = T)

df_all <- data.frame(Feature = as.character(), 
                     Destimate = as.numeric(), 
                     Pval1= as.numeric(),
                     Pval0 = as.numeric(),
                     n = as.numeric(), 
                     tree = as.character(),
                     zeroes = as.numeric(),
                     ones = as.numeric(),
                     Parameters_observed  = as.numeric(),
                     Parameters_MeanRandom  = as.numeric(),
                     Parameters_MeanBrownian = as.numeric(),
                     nPermut  = as.numeric()
)

index <- 0
for(fn in fns){
  index <- index + 1
  #  fn <- fns[8505]
  cat("I'm on", fn, ".\n")
  df_spec <-  try(expr = {qs::qread(fn)})
  
  if(class(df_spec) == "try-error"){
    cat("Error occurred on ", index,"out of", length(fns), ".", fn, ".\n")
    
  }else{
    df_all <- full_join(df_spec, df_all, by = c("Feature", "Destimate", "Pval1", "Pval0", "n", "tree", "zeroes", "ones", "Parameters_observed",
                                                "Parameters_MeanRandom", "Parameters_MeanBrownian", "nPermut")) 
  }
}

phylo_d_full <- df_all %>%  
  distinct() %>% 
  mutate(Destimate = ifelse(str_detect(tree, "glottolog") & n < ntips_half_glottolog, yes = NA, no = Destimate)) %>% 
  mutate(Destimate = ifelse(str_detect(tree, "gray") & n <  ntips_half_gray, yes = NA, no = Destimate)) %>%  
  mutate(Pval1 = ifelse(str_detect(tree, "glottolog") & n < ntips_half_glottolog, yes = NA, no = Pval1)) %>% 
  mutate(Pval1 = ifelse(str_detect(tree, "gray") & n < ntips_half_gray, yes = NA, no = Pval1)) %>%  
  mutate(Pval0 = ifelse(str_detect(tree, "glottolog") & n < ntips_half_glottolog, yes = NA, no = Pval0)) %>% 
  mutate(Pval0 = ifelse(str_detect(tree, "gray") & n < ntips_half_gray, yes = NA, no = Pval0)) %>%  
  rename(Feature_ID = Feature) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "ct"), "gray_mcct", "other")) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "glottolog"), "glottolog", tree_type)) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "posterior"), "gray_posteriors", tree_type)) %>% 
  mutate(one_is_one = ifelse(ones == 1 |zeroes== 1, "yes", "no")) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  mutate(min = ifelse(ones < zeroes, ones, zeroes)) %>% 
  mutate(min_p = min / (ones + zeroes)) %>% 
  mutate(summarise_col = ifelse(Pval0 > 0.05 &
                                  Pval1 > 0.05 & 
                                  Destimate < 0, 
                                "similar to both, below 0", NA)) %>%   
  mutate(summarise_col = if_else(Pval0 > 0.05 &
                                   Pval1 > 0.05 & 
                                   Destimate > 1, 
                                 "similar to both, above 1", summarise_col))   %>% 
  mutate(summarise_col = if_else(Pval0 > 0.05 &
                                   Pval1 > 0.05 & 
                                   between(Destimate, lower = 0, upper = 1), 
                                 "similar to both, between 0 & 1", summarise_col))   %>% 
  mutate(summarise_col = if_else(Pval0 > 0.05 &
                                   Pval1 < 0.05, 
                                 "similar to 0", summarise_col))   %>% 
  mutate(summarise_col = if_else(Pval0 < 0.05 &
                                   Pval1 > 0.05, 
                                 "similar to 1", summarise_col))  %>% 
  mutate(summarise_col = if_else(Pval0 < 0.05 &
                                   Pval1 < 0.05 & 
                                   between(Destimate, lower = 0, upper = 1), 
                                 "dissimilar to both, between 0 & 1", summarise_col)) %>% 
  
  mutate(summarise_col = ifelse(min == 0, "all same", summarise_col)) %>%   
  mutate(summarise_col = if_else(min == 1, "singleton", summarise_col))

phylo_d_full  %>% 
  write_tsv("output/HL_comparison/phylo_d/phylo_d_full.tsv", na = "")

### grouping over tree and feature
phylo_d_df <- phylo_d_full %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  group_by(tree_type, Feature_ID, Feature_tree) %>% 
  summarise(mean_D = mean(Destimate, na.rm= T),
            mean_Pval1 = mean(Pval1, na.rm= T),
            mean_Pval0 = mean(Pval0, na.rm= T), 
            ntip = mean(n), 
            n = n(),
            Parameters_observed = mean(Parameters_observed),
            Parameters_MeanRandom = mean(Parameters_MeanRandom),
            Parameters_MeanBrownian = mean(Parameters_MeanBrownian),
            zeroes = mean(zeroes), 
            min = mean(min),
            min_p = mean(min_p),
            ones = mean(ones), .groups = "drop") %>% 
  dplyr::select(mean_D, mean_Pval1, mean_Pval0, Feature_ID,  n, tree_type, ntip, min, min_p, Feature_tree, Parameters_observed, Parameters_MeanRandom, Parameters_MeanBrownian) %>% 
  mutate(summarise_col = ifelse(mean_Pval0 > 0.05 &
                                  mean_Pval1 > 0.05 & 
                                  mean_D < 0, 
                                "similar to both, below 0", NA)) %>%   
  mutate(summarise_col = if_else(mean_Pval0 > 0.05 &
                                   mean_Pval1 > 0.05 & 
                                   mean_D > 1, 
                                 "similar to both, above 1", summarise_col))   %>% 
  mutate(summarise_col = if_else(mean_Pval0 > 0.05 &
                                   mean_Pval1 > 0.05 & 
                                   between(mean_D, lower = 0, upper = 1), 
                                 "similar to both, between 0 & 1", summarise_col))   %>% 
  mutate(summarise_col = if_else(mean_Pval0 > 0.05 &
                                   mean_Pval1 < 0.05, 
                                 "similar to 0", summarise_col))   %>% 
  mutate(summarise_col = if_else(mean_Pval0 < 0.05 &
                                   mean_Pval1 > 0.05, 
                                 "similar to 1", summarise_col))  %>% 
  mutate(summarise_col = if_else(mean_Pval0 < 0.05 &
                                   mean_Pval1 < 0.05 & 
                                   between(mean_D, lower = 0, upper = 1), 
                                 "dissimilar to both, between 0 & 1", summarise_col)) %>% 
  
  mutate(summarise_col = ifelse(min == 0, "all same", summarise_col)) %>%   
  mutate(summarise_col = if_else(min == 1, "singleton", summarise_col))

phylo_d_df %>% 
  write_tsv("output/HL_comparison/phylo_d/phylo_d_df.tsv", na = "")

