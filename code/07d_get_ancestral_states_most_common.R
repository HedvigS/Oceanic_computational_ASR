source("01_requirements.R")

GB_df_long <- read_tsv(GB_binary_fn, show_col_types = F) %>% 
  dplyr::select(-na_prop) %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  filter(!is.na(value))

glottolog_df <- read_tsv(glottolog_df_fn, show_col_types = F)  %>% 
  dplyr::select(Language_ID = Glottocode, classification)

not_enough_languages_df <- GB_df_long %>% 
  group_by(variable) %>% 
  summarise(n = n()) %>%
  mutate(not_enough = ifelse(n <= ntips_half_glottolog , "Not enough languages", "")) %>% 
  rename(Feature_ID = variable)

east_polynesian_df <- GB_df_long %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  filter(str_detect(classification, "east2449")) %>% 
  mutate(value = as.character(value)) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  complete(variable, value, fill = list(n = 0)) %>% 
  group_by(variable) %>% 
  mutate(all = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(percentage = n / all) %>% 
  reshape2::dcast(variable~value, value.var = "percentage") %>% 
  mutate(`Proto-language` = "Proto-Eastern Polynesian")

east_polynesian_df$most_common_prediction <- if_else(east_polynesian_df$`0` > 0.6, "Absent", if_else(east_polynesian_df$`1` > 0.6, "Present", "Half")) 

polynesian_df <- GB_df_long %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  filter(str_detect(classification, "poly1242")) %>%
  mutate(value = as.character(value)) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  complete(variable, value, fill = list(n = 0)) %>% 
  group_by(variable) %>% 
  mutate(all = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(percentage = n / all) %>% 
  reshape2::dcast(variable~value, value.var = "percentage") %>% 
  mutate(`Proto-language` = "Proto-Polynesian")

polynesian_df$most_common_prediction <- if_else(polynesian_df$`0` > 0.6, "Absent", if_else(polynesian_df$`1` > 0.6, "Present", "Half")) 

central_pacific_df <- GB_df_long %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  filter(str_detect(classification, "cent2060")) %>% 
  mutate(value = as.character(value)) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  complete(variable, value, fill = list(n = 0)) %>% 
  group_by(variable) %>% 
  mutate(all = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(percentage = n / all) %>% 
  reshape2::dcast(variable~value, value.var = "percentage") %>% 
  mutate(`Proto-language` = "Proto-Central Pacific")

central_pacific_df$most_common_prediction <- if_else(central_pacific_df$`0` > 0.6, "Absent", if_else(central_pacific_df$`1` > 0.6, "Present", "Half")) 

oceanic_df <- GB_df_long %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  filter(str_detect(classification, "ocea1241")) %>% 
  mutate(value = as.character(value)) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  complete(variable, value, fill = list(n = 0)) %>% 
  group_by(variable) %>% 
  mutate(all = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(percentage = n / all) %>% 
  reshape2::dcast(variable~value, value.var = "percentage") %>% 
  mutate(`Proto-language` = "Proto-Oceanic")

oceanic_df$most_common_prediction <- if_else(oceanic_df$`0` > 0.6, "Absent", if_else(oceanic_df$`1` > 0.6, "Present", "Half")) 

all_df <- oceanic_df  %>% 
  full_join(central_pacific_df, by = c("variable", "0", "1", "Proto-language", "most_common_prediction")) %>% 
  full_join(polynesian_df, by = c("variable", "0", "1", "Proto-language", "most_common_prediction")) %>% 
  full_join(east_polynesian_df, by = c("variable", "0", "1", "Proto-language", "most_common_prediction")) %>% 
  dplyr::rename(Feature_ID = variable)

HL_findings_sheet <- read_tsv(HL_findings_sheet_fn) %>% 
  full_join(read_csv(HL_findings_sheet_conflicts_fn) %>% 
              rename(Prediction = Value), by = c("Proto-language", "Feature_ID", "Prediction")          ) %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`)

values_df <- all_df %>% 
  right_join(HL_findings_sheet, by = c("Feature_ID", "Proto-language")) %>% 
  distinct(Feature_ID,`Proto-language`, Prediction, most_common_prediction)

values_df$result_most_common <- ifelse(values_df$most_common_prediction == "Present" & values_df$Prediction == 1, "True Positive",  
                                       ifelse(values_df$most_common_prediction == "Absent" & values_df$Prediction == 0, "True Negative",   
                                              ifelse(values_df$most_common_prediction == "Absent" & values_df$Prediction == 1, "False Negative",  
                                                     ifelse(values_df$most_common_prediction == "Present" & values_df$Prediction == 0, "False Positive",
                                                            
                                                            ifelse(values_df$most_common_prediction == "Half", "Half", NA)))))

#adding in a col for which value is the lowest


lowest_df <- GB_df_long %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  filter(str_detect(classification, "ocea1241")) %>% 
  mutate(value = as.character(value)) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(n = n()) %>% 
  group_by(variable) %>% 
  mutate(ntip = sum(n)) %>% 
  slice_min(order_by = n, n = 1) %>% 
  rename(min = n, Feature_ID = variable)

all_df  %>% 
  left_join(values_df, by = c("Feature_ID", "Proto-language", "most_common_prediction")) %>% 
  left_join(not_enough_languages_df, by = "Feature_ID") %>% 
  mutate(most_common_prediction = ifelse(not_enough == "Not enough languages", "Not enough languages", most_common_prediction)) %>% 
  mutate(result_most_common = ifelse(not_enough == "Not enough languages", "Not enough languages", result_most_common)) %>% 
  dplyr::select(-not_enough, -n) %>% 
  left_join(lowest_df, by = "Feature_ID") %>% 
  write_tsv("output/HL_comparison/most_common_reconstructions.tsv")
