source("01_requirements.R")

HL_findings_sheet <- read_csv(HL_findings_sheet_conflicts_fn) %>% 
  rename(Prediction = Value) %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`, Source, Feature)

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  filter(!is.na(`result_most_common`)) %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, "most_common_prediction", "Proto-language")

#reading in the results from each method and tree and calculating the number of true negatives etc

parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, `Parsimony result (Glottolog-tree)`, "Proto-language")

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>%
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, `Parsimony result (Gray et al 2009-tree)`, "Proto-language")

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  right_join(HL_findings_sheet) %>%
  dplyr::select(Feature_ID, `Parsimony result (Gray et al 2009-tree posteriors)`, "Proto-language")

ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, `ML result (Glottolog-tree)`, "Proto-language")

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, `ML result (Gray et al 2009-tree)`, "Proto-language")

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, `ML result (Gray et al 2009-tree posteriors)`, "Proto-language")

  

full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_df) %>%
  distinct()
#  dplyr::select(Feature_ID, `Proto-language`, Prediction, Source,`Parsimony result (Glottolog-tree)`, `Parsimony result (Gray et al 2009-tree) mcct`, `Parsimony result (Gray et al 2009-tree) posteriors`,  `ML result (Glottolog-tree)`, `ML result (Gray et al 2009-tree) mcct`,`ML result (Gray et al 2009-tree) posteriors`, result_most_common) %>% 
  distinct()

full_df[full_df == "True Positive"] <- "Agree"
full_df[full_df == "True Negative"] <- "Agree"
full_df[full_df == "False Positive"] <- "Disagree"
full_df[full_df == "False Negative"] <- "Disagree"


full_df %>% xtable()
