source("01_requirements.R")

GB_df_long <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) %>% 
  reshape2::melt(id.vars = "Language_ID") %>% 
  filter(!is.na(value))

glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Language_ID = Glottocode, classification)

east_polynesian_df <- GB_df_long %>% 
  left_join(glottolog_df) %>% 
  filter(str_detect(classification, "east2449")) %>% 
  mutate(value = as.character(value)) %>% 
  mutate(value = str_replace_all(value, "1", "Present")) %>% 
  mutate(value = str_replace_all(value, "0", "Absent")) %>% 
  group_by(variable, value) %>% 
  dplyr::summarise(n = n()) %>% 
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
  dplyr::summarise(n = n()) %>% 
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
  dplyr::summarise(n = n()) %>% 
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
  dplyr::summarise(n = n()) %>% 
  dplyr::top_n(1, wt = n) %>% 
  mutate(dup = duplicated(variable) + duplicated(variable, fromLast = T)) %>% 
  mutate(value = ifelse(dup >= 1,"Half", value )) %>% 
  distinct(variable, value) %>% 
  mutate(`Proto-language` = "Proto-Oceanic")


all_df <- oceanic_df  %>% 
  full_join(central_pacific_df) %>% 
  full_join(polynesian_df) %>% 
  full_join(east_polynesian_df) %>% 
  dplyr::rename(Feature_ID = variable)


HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`)

values_df <- all_df %>% 
  right_join(HL_findings_sheet) %>% 
  distinct(Feature_ID,`Proto-language`, Prediction, value)

values_df$result_most_common <- ifelse(values_df$value == "Present" & values_df$Prediction == 1, "True Positive",  
                                       ifelse(values_df$value == "Absent" & values_df$Prediction == 0, "True Negative",   
                                              ifelse(values_df$value == "Absent" & values_df$Prediction == 1, "False Negative",  
                                                     ifelse(values_df$value == "Present" & values_df$Prediction == 0, "False Positive",
                                                            
                                                            ifelse(values_df$value == "Half", "Half", NA)))))
all_df  %>% 
  left_join(values_df) %>% 
  dplyr::select(-Prediction) %>% 
  write_tsv("output/HL_comparison/most_common_reconstructions.tsv")
