source("01_requirements.R")

full_df <- read_tsv("output/all_reconstructions_all_methods_long.tsv")  

prediction_df <-  full_df %>% 
  filter(!is.na(value)) %>% 
  filter(is.na(conflict)) %>% 
  filter(variable == "prediction") %>% 
  dplyr::select(-conflict, -variable) %>% 
  unite(method, tree_type, col = "method", sep = " - ") %>% 
  mutate(method = ifelse(method == "most_common - most_common", "most common", method)) %>% 
  mutate(method = ifelse(method == "HL - HL", "HL", method)) %>% 
  mutate(method = str_replace_all(method, "_", " "))

left <- prediction_df %>% 
  rename(value_left = value, method_left =  method)

right <- prediction_df  %>% 
  rename(value_right = value, method_right =  method)

joined <- full_join(left, right, by = c("Feature_ID", "Proto-language")) 

joined_diff_df <-  joined %>% 
  mutate(diff = ifelse(value_right == "Absent" & value_left == "Absent", "True", NA)) %>% 
  mutate(diff = ifelse(value_right == "Present" & value_left == "Present", "True", diff)) %>%
  mutate(diff = ifelse(value_right == "Half" & value_left == "Half", "True", diff)) %>% 
  mutate(diff = ifelse(value_right == "Absent" & value_left == "Present", "False", diff)) %>%  
  mutate(diff = ifelse(value_right == "Present" & value_left == "Absent", "False", diff)) %>%  
  mutate(diff = ifelse(value_right == "Present" & value_left == "Half", "Half", diff)) %>% 
  mutate(diff = ifelse(value_right == "Absent" & value_left == "Half", "Half", diff)) %>% 
  mutate(diff = ifelse(value_right == "Half" & value_left == "Present", "Half", diff)) %>% 
  mutate(diff = ifelse(value_right == "Half" & value_left == "Absent", "Half", diff)) 

#joined_diff_df %>% 
#  filter(variable == "prediction_1")

