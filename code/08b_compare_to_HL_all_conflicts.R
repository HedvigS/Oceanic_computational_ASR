source("01_requirements.R")

#First we make a df with the basic counts for all oceanic languages per feature and what the distribution at the tips are. This will help us identify cases where all tips where of the same kind. These cases are currently excluded from the ML workflow since corHMM doesn't accept trees where the values are the same at all tips. The parsimony function however accepts such cases, so in order to make them comparable we need to put them back in to the results. We can use the basic summary table from the parsimony glottolog analysis for this since that per definition includes all lgs. Note that the total distribution over all lgs isn't the same as the distribution in the gray tree, which may be missing specific tips which have the divergent value.
values_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  distinct(Feature_ID, ntips = ntips_parsimony_glottolog, zeroes_total= zeroes_parsimony_glottolog ,ones_total=  ones_parsimony_glottolog, min_percent = min_percent_parsimony_glottolog)

HL_findings_sheet <- read_csv("data/HL_findings/HL_findings_conflicts.csv") %>% 
  rename(Prediction = Value) %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`, Source, Feature)

values_df <- values_df %>% 
  right_join(HL_findings_sheet) %>% 
  distinct(Feature_ID, zeroes_total, ones_total, `Proto-language`, Prediction, Source, Feature)

values_df$most_common_value <- ifelse(values_df$zeroes_total > values_df$ones_total, 0, 1)

values_df$result_most_common <- ifelse(values_df$most_common_value == 1 & values_df$Prediction == 1, "True Positive",  
                                                                                  ifelse(values_df$most_common_value == 0 & values_df$Prediction == 0, "True Negative",   
                                                                                          ifelse(values_df$most_common_value == 0 & values_df$Prediction == 1, "False Negative",  
                                                                                                  ifelse(values_df$most_common_value == 1 & values_df$Prediction == 0, "False Positive", NA))))
                                       
#reading in the results from each method and tree and calculating the number of true negatives etc

parsimony_glottolog_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  inner_join(values_df) %>%  
  dplyr::select(Feature_ID, `Proto-language`, `Parsimony result (Glottolog-tree)`, Prediction, Source)

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  inner_join(values_df) %>% 
  dplyr::select(Feature_ID, `Proto-language`, `Parsimony result (Gray et al 2009-tree) mcct` = `Parsimony result (Gray et al 2009-tree)`, Prediction, Source)

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/results_by_tree/all_reconstructions_aggregate.tsv")   %>% 
  right_join(values_df) %>% 
  dplyr::select(Feature_ID, `Proto-language`, `Parsimony result (Gray et al 2009-tree) posteriors` = `parsimony result (Gray et al 2009-tree)`, Prediction, Source)

ML_glottolog_df <- read_tsv("output/glottolog_tree_binary/ML/all_reconstructions.tsv") %>% 
  right_join(values_df) %>% 
  dplyr::select(Feature_ID, `Proto-language` ,`ML result (Glottolog-tree)`, Prediction, Source) 

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  right_join(values_df) %>% 
  dplyr::select(Feature_ID, `Proto-language`,`ML result (Gray et al 2009-tree) mcct` = `ML result (Gray et al 2009-tree)`, Prediction, Source)


ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/results_by_tree/all_reconstructions_aggregate.tsv") %>% 
  right_join(values_df) %>% 
  dplyr::select(Feature_ID, `Proto-language`,`ML result (Gray et al 2009-tree) posteriors` = `ML result (Gray et al 2009-tree)`, Prediction, Source) %>% 
  distinct()

full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(values_df) %>%
  dplyr::select(Feature_ID, `Proto-language`, Prediction, Source,`Parsimony result (Glottolog-tree)`, `Parsimony result (Gray et al 2009-tree) mcct`, `Parsimony result (Gray et al 2009-tree) posteriors`,  `ML result (Glottolog-tree)`, `ML result (Gray et al 2009-tree) mcct`,`ML result (Gray et al 2009-tree) posteriors`, result_most_common) %>% 
  distinct()

full_df[full_df == "True Positive"] <- "Agree"
full_df[full_df == "True Negative"] <- "Agree"
full_df[full_df == "False Positive"] <- "Disagree"
full_df[full_df == "False Negative"] <- "Disagree"


full_df %>% xtable()
