source("01_requirements.R")

dists <- read_tsv("output/HL_comparison/dist_hl_comparison.tsv") %>% 
  filter(score == "HL_prediction") %>% 
  mutate(score = ifelse(score == "HL_prediction", "dist", NA)) %>% 
  dplyr::select(-HL_prediction) %>% 
  dplyr::select(score , "Parsimony_glottolog"  = glottolog_parsimony_prediction,     "Parsimony_gray_mcct" = gray_mcct_parsimony_prediction,       "Parsimony_gray_posteriors"= gray_posteriors_parsimony_prediction,  "ML_glottolog"        = glottolog_ML_prediction,       "ML_gray_mcct"     = gray_mcct_ML_prediction    ,     "ML_gray_posteriors"   = gray_posteriors_ML_prediction,      "most_common"    = most_common_prediction)

dists_new <- read_tsv("output/HL_comparison/dist_hl_comparison_new.tsv") %>% 
  filter(score == "HL_prediction") %>% 
  mutate(score = ifelse(score == "HL_prediction", "dist_new", NA)) %>% 
  dplyr::select(-HL_prediction) %>% 
  dplyr::select(score, "Parsimony_glottolog"  = glottolog_parsimony_prediction,     "Parsimony_gray_mcct" = gray_mcct_parsimony_prediction,       "Parsimony_gray_posteriors"= gray_posteriors_parsimony_prediction,  "ML_glottolog"        = glottolog_ML_prediction,       "ML_gray_mcct"     = gray_mcct_ML_prediction    ,     "ML_gray_posteriors"   = gray_posteriors_ML_prediction,      "most_common"    = most_common_prediction)

accuracy_scores <- read_tsv("output/HL_comparison/accuracy_tables.tsv") 

table <- accuracy_scores %>% 
  full_join(dists) %>% 
  full_join(dists_new) %>% 
  column_to_rownames("score") %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(dist_new, F1_score, F1_score_incl_half, Accuracy, Accuracy_incl_half) %>% 
  mutate(dist_new = 1 - dist_new)



table$Accuracy_Rank<-rank(table$Accuracy, ties.method = "min")
table$F1_score_Rank<-rank(table$F1_score, ties.method = "min")
table$F1_score_incl_half_Rank<-rank(table$F1_score_incl_half, ties.method = "min")
table$Accuracy_incl_half_Rank<-rank(table$Accuracy_incl_half, ties.method = "min")
table$dist_new_Rank<-rank(table$dist_new, ties.method = "min")


