source("01_requirements.R")

full_df <- read_tsv("output/all_reconstructions_all_methods_long.tsv") %>%  
  filter(!is.na(value)) %>% 
  filter(is.na(conflict)) %>% 
#  filter(!str_detect(method, "HL")) %>% 
  filter(variable == "prediction") %>% 
  dplyr::select(-conflict, -variable) %>% 
  unite(method, tree_type, col = "method", sep = " - ") %>% 
  mutate(method = ifelse(method == "most_common - most_common", "most common", method)) %>% 
  mutate(method = ifelse(method == "HL - HL", "HL", method)) %>% 
  mutate(method = str_replace_all(method, "_", " "))

left <- full_df %>% 
  rename(value_left = value, method_left =  method)

right <- full_df %>% 
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
  mutate(diff = ifelse(value_right == "Half" & value_left == "Absent", "Half", diff)) %>% 
  group_by(method_left, method_right, diff) %>% 
  summarise(diff_n = n(),   .groups = "drop") %>% 
  group_by(method_left, method_right) %>% 
        mutate(reconstructions_all = sum(diff_n)) %>% 
        mutate(diff_n = ifelse(diff == "Half", diff_n /2, diff_n)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "diff", values_from = "diff_n") 

#same to same distance could be set to 1, but it 
#joined_diff_df[is.na(joined_diff_df)] <- 1

matrix <- joined_diff_df %>% 
  mutate(score = ((True + Half) / reconstructions_all)) %>% 
  dplyr::select(method_left, method_right, score) %>% 
  reshape2::dcast(method_left ~method_right, value.var = "score") %>% 
  dplyr::select(method_left, "parsimony - glottolog", "parsimony - gray mcct", "parsimony - gray posteriors", "ML - glottolog", "ML - gray mcct", "ML - gray posteriors", "most common", "HL") 

matrix$method_left <- factor(matrix$method_left, levels = c("parsimony - glottolog", "parsimony - gray mcct", "parsimony - gray posteriors", "ML - glottolog", "ML - gray mcct", "ML - gray posteriors", "most common", "HL"))

matrix <- matrix %>% 
  arrange(method_left) %>%
  column_to_rownames("method_left") %>%
  as.matrix() 
  
png(filename = paste0(OUTPUTDIR_plots, "/results/accuracy_score_heatmap_all_methods.png"),   width = 8, height = 8, units = "in", res = 400)

matrix %>% 
  heatmap.2(  key = F, 
              symm = T,
              dendrogram = "none",
              revC = T,
              Rowv = F,
              Colv = F,
              lwid=c(0.1,4), lhei=c(0.1,4),
              trace = "none", cellnote = round(matrix , 2),
              margin=c(20,20), 
              col=viridis(15, direction = -1))

x <- dev.off()




  