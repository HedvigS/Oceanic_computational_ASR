source("01_requirements.R")

#First we make a df with the basic counts for all oceanic languages per feature and what the distribution at the tips are. This will help us identify cases where all tips where of the same kind. These cases are currently excluded from the ML workflow since corHMM doesn't accept trees where the values are the same at all tips. The parsimony function however accepts such cases, so in order to make them comparable we need to put them back in to the results. We can use the basic summary table from the parsimony glottolog analysis for this since that per definition includes all lgs. Note that the total distribution over all lgs isn't the same as the distribution in the gray tree, which may be missing specific tips which have the divergent value.
values_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  distinct(Feature_ID, ntips = ntips_parsimony_glottolog, zeroes_total= zeroes_parsimony_glottolog ,ones_total=  ones_parsimony_glottolog, min_percent = min_percent_parsimony_glottolog)

HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`)

values_df <- values_df %>% 
  right_join(HL_findings_sheet) %>% 
  distinct(Feature_ID, zeroes_total, ones_total, `Proto-language`, Prediction)

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv")

most_common_df_summarised <- most_common_df %>% 
  inner_join(values_df) %>% 
  dplyr::rename(variable = `result_most_common`) %>% 
  group_by(variable) %>% 
  summarise(most_common = n())

#reading in the results from each method and tree and calculating the number of true negatives etc

parsimony_glottolog_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  inner_join(values_df) %>% 
  dplyr::rename(variable = `Parsimony result (Glottolog-tree)`) %>% 
  group_by(variable) %>% 
  summarise(Parsimony_glottolog = n())

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  inner_join(values_df) %>% 
  dplyr::rename(variable = `Parsimony result (Gray et al 2009-tree)`) %>% 
  group_by(variable) %>% 
  summarise(Parsimony_gray_mcct = n())

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/results_by_tree/all_reconstructions_aggregate.tsv")   %>% 
  filter(!is.na(Prediction)) %>% 
  right_join(values_df) %>% 
  dplyr::rename(variable = `parsimony result (Gray et al 2009-tree)`) %>% 
  group_by(variable) %>% 
  summarise(Parsimony_gray_posteriors = n())

ML_glottolog_df <- read_tsv("output/glottolog_tree_binary/ML/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  right_join(values_df) %>% 
  dplyr::rename(variable = `ML result (Glottolog-tree)`) %>% 
  group_by(variable) %>% 
  summarise(ML_glottolog = n()) 

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  right_join(values_df) %>% 
  left_join(most_common_df) %>% 
  mutate(`ML result (Gray et al 2009-tree)` = ifelse(is.na(`ML result (Gray et al 2009-tree)`) & Prediction == 0 & result_most_common == "True Negative", "True Negative", `ML result (Gray et al 2009-tree)`) ) %>% 
                               dplyr::rename(variable = `ML result (Gray et al 2009-tree)`) %>% 
  group_by(variable) %>% 
  summarise(ML_gray_mcct = n()) 

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/results_by_tree/all_reconstructions_aggregate.tsv") %>% 
  dplyr::select(-Prediction) %>% 
  right_join(values_df) %>% 
  left_join(most_common_df) %>% 
  mutate(`ML result (Gray et al 2009-tree)` = if_else(is.na(`ML result (Gray et al 2009-tree)`) & Prediction == 0 & result_most_common == "True Negative", "True Negative", `ML result (Gray et al 2009-tree)`) ) %>% 
  dplyr::rename(variable = `ML result (Gray et al 2009-tree)`) %>% 
  group_by(variable) %>% 
  summarise(ML_gray_posteriors = n()) 


full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
 full_join(most_common_df_summarised)

full_df[is.na(full_df)] <- 0

accuracy_tables <- full_df %>% 
  column_to_rownames("variable") %>% 
  t() %>%
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  group_by(variable) %>% 
  mutate(Disagree = sum(`False Negative`, `False Positive`, na.rm = T),
       Agree = sum(`True Negative` , `True Positive`, na.rm = T), 
       reconstructions_non_half = sum(Agree, Disagree, na.rm = T),
       reconstructions_all= sum(Agree, Disagree, Half, na.rm = T)) %>% 
  mutate(Accuracy = Agree / reconstructions_all, 
         Accuracy_incl_half = ((Agree + (Half/2)) / reconstructions_all), 
         Precision = `True Positive` / (`True Positive` + `False Positive` + Half), 
         Precision_incl_half = (`True Positive` + (Half*0.5)) / (`True Positive` + `False Positive`+ Half), 
         Recall = `True Positive` / (`True Positive` + `False Negative` + Half),
        Recall_incl_half = (`True Positive`+ (Half*0.5) )/ (`True Positive` + `False Negative` + Half)) %>%
    mutate(F1_score = 2 * ((Precision*Recall)/(Precision + Recall)), 
           F1_score_incl_half = 2 * ((Precision_incl_half *Recall_incl_half)/(Precision_incl_half + Recall_incl_half)) )%>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  column_to_rownames("variable") %>% 
  t() 

accuracy_tables %>% 
  as.data.frame() %>% 
  rownames_to_column("score") %>% 
  write_tsv("output/HL_comparison/accuracy_tables.tsv")

accuracy_tables[10:18,] %>% 
  heatmap.2( key = F,
              dendrogram = "none",
              revC = T,
              trace = "none", 
             Rowv = F,
             Colv = F,
            cellnote = round(accuracy_tables[10:18,] , 2),
              margin=c(20,20), col=viridis(15, direction = -1))


