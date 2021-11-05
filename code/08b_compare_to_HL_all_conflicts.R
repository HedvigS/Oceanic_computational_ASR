source("01_requirements.R")

HL_findings_sheet <- read_csv("data/HL_findings/HL_findings_conflicts.csv") %>% 
  rename(Prediction = Value) %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`, Source, Feature)

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  dplyr::select(-value) %>% 
  inner_join(HL_findings_sheet) 

#reading in the results from each method and tree and calculating the number of true negatives etc

parsimony_glottolog_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  inner_join(HL_findings_sheet)  %>% 
  dplyr::select(Feature_ID, `Proto-language`, `Parsimony result (Glottolog-tree)`, Prediction, Source)

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  inner_join(HL_findings_sheet)  %>% 
  dplyr::select(Feature_ID, `Proto-language`, `Parsimony result (Gray et al 2009-tree) mcct` = `Parsimony result (Gray et al 2009-tree)`, Prediction, Source)

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/results_by_tree/all_reconstructions_aggregate.tsv")   %>% 
  inner_join(HL_findings_sheet)  %>% 
  dplyr::select(Feature_ID, `Proto-language`, `Parsimony result (Gray et al 2009-tree) posteriors` = `parsimony result (Gray et al 2009-tree)`, Prediction, Source)

ML_glottolog_df <- read_tsv("output/glottolog_tree_binary/ML/all_reconstructions.tsv") %>% 
  inner_join(HL_findings_sheet)  %>% 
  dplyr::select(Feature_ID, `Proto-language` ,`ML result (Glottolog-tree)`, Prediction, Source) 

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  inner_join(HL_findings_sheet)  %>% 
  dplyr::select(Feature_ID, `Proto-language`,`ML result (Gray et al 2009-tree) mcct` = `ML result (Gray et al 2009-tree)`, Prediction, Source)

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/results_by_tree/all_reconstructions_aggregate.tsv") %>% 
  inner_join(HL_findings_sheet)  %>% 
  dplyr::select(Feature_ID, `Proto-language`,`ML result (Gray et al 2009-tree) posteriors` = `ML result (Gray et al 2009-tree)`, Prediction, Source) %>% 
  distinct()

full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_df) %>%
  dplyr::select(Feature_ID, `Proto-language`, Prediction, Source,`Parsimony result (Glottolog-tree)`, `Parsimony result (Gray et al 2009-tree) mcct`, `Parsimony result (Gray et al 2009-tree) posteriors`,  `ML result (Glottolog-tree)`, `ML result (Gray et al 2009-tree) mcct`,`ML result (Gray et al 2009-tree) posteriors`, result_most_common) %>% 
  distinct()

full_df[full_df == "True Positive"] <- "Agree"
full_df[full_df == "True Negative"] <- "Agree"
full_df[full_df == "False Positive"] <- "Disagree"
full_df[full_df == "False Negative"] <- "Disagree"


full_df %>% xtable()
