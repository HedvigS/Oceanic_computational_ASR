source("01_requirements.R")

df <- read_tsv("output/conservatism/all_reconstructions.tsv")  

#analyzing accuracy taking into account the probability of ML
Abscence_accuray_melted <- df %>% 
  filter(Prediction == 0) %>%
  dplyr::select(Feature_ID, glottolog_parsimony_prediction_0, gray_parsimony_prediction_0, glottolog_ML_prediction_0, gray_ML_prediction_0) %>% 
  reshape2::melt(id.vars = "Feature_ID") 

Presence_accuray_melted <- df %>% 
  filter(Prediction == 1) %>%
  dplyr::select(Feature_ID, glottolog_parsimony_prediction_1, gray_parsimony_prediction_1, glottolog_ML_prediction_1, gray_ML_prediction_1) %>% 
  reshape2::melt(id.vars = "Feature_ID") 

accuracy_scores <- Presence_accuray_melted %>% 
  full_join(Abscence_accuray_melted) %>% 
  mutate(variable = variable %>% str_replace_all("_1", "")) %>% 
  mutate(variable = variable %>% str_replace_all("_0", "")) %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value, na.rm = T))

#analysing accuracy and F1 score with the rounding up, i.e. not taking into account probability

accuracy_summary_table<- df %>% 
  filter(!is.na(Prediction)) %>% 
  separate(Abbreviation, into = c("`Feature_ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select("Feature_ID" , 
                `Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` ,
                `Parsimony result (Gray et al 2009-tree)`,
                `ML result (Gray et al 2009-tree)`) %>%
  reshape2::melt(id.vars = "Feature_ID" ) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  reshape2::dcast(variable ~ value, value.var = "n") %>% 
  group_by(variable) %>% 
  mutate(Disagree = sum(`False Negative`, `False Positive`, na.rm = T),
         Agree = sum(`True Negative` , `True Positive`, na.rm = T), 
         reconstructions_non_half = sum(Agree, Disagree, na.rm = T),
         reconstructions_all= sum(Agree, Disagree, Half, na.rm = T)) %>% 
  mutate(Accuracy = Agree / reconstructions_non_half, 
         Accuracy_incl_half = ((Agree + (Half/2)) / reconstructions_all), 
         Precision = `True Positive` / (`True Positive` + `False Positive`), 
         Recall = `True Positive` / (`True Positive` + `False Negative`)) %>%
  mutate(F1_score = 2 * ((Precision*Recall)/(Precision + Recall))) %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  t() 

colnames(accuracy_summary_table) <- accuracy_summary_table[1,]    
accuracy_summary_table <- accuracy_summary_table[-1,]    

accuracy_summary_table %>% 
  as.data.frame() %>% 
  rownames_to_column("Stat") %>% 
  write_tsv("output/HL_comparison/HL_comparison_summary_mcct.tsv")

##new predictions

df_new_preductions_positive <-  df %>%
  filter(is.na(Prediction)) %>% 
  filter(glottolog_ML_prediction == "Present") %>% 
  filter(glottolog_parsimony_prediction == "Present") %>% 
  filter(gray_ML_prediction == "Present") %>% 
  filter(gray_parsimony_prediction == "Present") %>%
  filter(min_percent_ML_glottolog > 0.3) %>% 
  dplyr::select(Feature_ID, `Proto-language`, automatic_prediction = gray_parsimony_prediction) %>% 
  left_join(GB_df_desc) 


##CONFLICTS

df_conflict_resolution <- HL_findings_sheet_conflict %>% 
  full_join(automatic_predctions, by = c("Feature_ID", "Proto-language")) 

# 
# df_pruned_erg <- df %>% 
#   filter(!is.na(Prediction)) %>% 
#   arrange(`Proto-language`, Feature_ID) %>% 
#   filter(Feature_ID == "GB409" |
#            Feature_ID == "GB408" ) %>% 
#   #      `Feature_ID` == "GB410" |
#   #     `Feature_ID` == "GB147" )   %>% 
#   left_join(GB_df_desc) %>% 
#   separate(Abbreviation, into = c("Feature_ID", "Abbreviation"), sep = " ") %>% 
#   dplyr::select(Feature_ID, Abbreviation, Question, "Proto-language", `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
#                 `ML result (Glottolog-tree)` ,
#                 `Parsimony result (Gray et al 2009-tree)`,
#                 `ML result (Gray et al 2009-tree)`)
# 
# df_pruned_erg %>% 
#   write_tsv("output/HL_comparison/HL_comparison_erg.tsv")
