source("01_requirements.R")

GB_df_long <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  filter(!is.na(value))

GB_df_long %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>% View()

glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Language_ID = Glottocode, classification)

east_polynesian_df <- GB_df_long %>% 
  left_join(glottolog_df) %>% 
  filter(str_detect(classification, "east2449")) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_replace_all(value, "1", "Present")) %>% 
  mutate(value = str_replace_all(value, "0", "Absent")) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  dplyr::top_n(1, wt = n) %>% 
  mutate(dup = duplicated(variable) + duplicated(variable, fromLast = T)) %>% 
  mutate(value = ifelse(dup >= 1,"Half", value )) %>% 
  distinct(variable, value) %>% 
  mutate(`Proto-language` = "Proto-Eastern Polynesian")


polynesian_df <- GB_df_long %>% 
  left_join(glottolog_df) %>% 
  filter(str_detect(classification, "poly1242")) %>%
  mutate(value = as.character(value)) %>% 
  mutate(value = str_replace_all(value, "1", "Present")) %>% 
  mutate(value = str_replace_all(value, "0", "Absent")) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  dplyr::top_n(1, wt = n) %>% 
  mutate(dup = duplicated(variable) + duplicated(variable, fromLast = T)) %>% 
  mutate(value = ifelse(dup >= 1,"Half", value )) %>% 
  distinct(variable, value) %>% 
  mutate(`Proto-language` = "Proto-Polynesian")

central_pacific_df <- GB_df_long %>% 
  left_join(glottolog_df) %>% 
  filter(str_detect(classification, "cent2060")) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_replace_all(value, "1", "Present")) %>% 
  mutate(value = str_replace_all(value, "0", "Absent")) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  dplyr::top_n(1, wt = n) %>% 
  mutate(dup = duplicated(variable) + duplicated(variable, fromLast = T)) %>% 
  mutate(value = ifelse(dup >= 1,"Half", value )) %>% 
  distinct(variable, value) %>% 
  mutate(`Proto-language` = "Proto-Central Pacific")



oceanic_df <- GB_df_long %>% 
  left_join(glottolog_df) %>% 
  filter(str_detect(classification, "ocea1241")) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_replace_all(value, "1", "Present")) %>% 
  mutate(value = str_replace_all(value, "0", "Absent")) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  dplyr::top_n(1, wt = n) %>% 
  mutate(dup = duplicated(variable) + duplicated(variable, fromLast = T)) %>% 
  mutate(value = ifelse(dup >= 1,"Half", value )) %>% 
  distinct(variable, value) %>% 
  mutate(`Proto-language` = "Proto-Oceanic")


  











#First we make a df with the basic counts for all oceanic languages per feature and what the distribution at the tips are. This will help us identify cases where all tips where of the same kind. These cases are currently excluded from the ML workflow since corHMM doesn't accept trees where the values are the same at all tips. The parsimony function however accepts such cases, so in order to make them comparable we need to put them back in to the results. We can use the basic summary table from the parsimony glottolog analysis for this since that per definition includes all lgs. Note that the total distribution over all lgs isn't the same as the distribution in the gray tree, which may be missing specific tips which have the divergent value.
values_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  distinct(Feature_ID, ntips = ntips_parsimony_glottolog, zeroes_total= zeroes_parsimony_glottolog ,ones_total=  ones_parsimony_glottolog, min_percent = min_percent_parsimony_glottolog)

HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`)

values_df <- values_df %>% 
  right_join(HL_findings_sheet) %>% 
  distinct(Feature_ID, zeroes_total, ones_total, `Proto-language`, Prediction)

values_df$most_common_value <- ifelse(values_df$zeroes_total > values_df$ones_total, 0, 1)

values_df$result_most_common <- ifelse(values_df$most_common_value == 1 & values_df$Prediction == 1, "True Positive",  
                                       ifelse(values_df$most_common_value == 0 & values_df$Prediction == 0, "True Negative",   
                                              ifelse(values_df$most_common_value == 0 & values_df$Prediction == 1, "False Negative",  
                                                     ifelse(values_df$most_common_value == 1 & values_df$Prediction == 0, "False Positive",
                                                            
                                                            ifelse(values_df$most_common_value == "Half", "Half", NA)))))
most_common_df <- values_df %>% 
  dplyr::rename(variable = result_most_common) %>% 
  group_by(variable) %>% 
  summarise(Most_common_value = n())
