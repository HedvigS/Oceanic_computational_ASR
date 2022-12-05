source("01_requirements.R")

fns <- list.files("output/HL_comparison/", pattern = "phylo_d.*tsv", full.names = T)

phylo_d_df <- fns %>% 
  map_df(
    function(x) data.table::fread(x ,
                                  encoding = 'UTF-8', header = TRUE, 
                                  fill = TRUE, blank.lines.skip = TRUE,
                                  sep = "\t", na.strings = "",
    ) 
      ) %>% 
  distinct() %>% 
  rename(Feature_ID = Feature) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "ct"), "gray_mcct", "other")) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "glottolog"), "glottolog", tree_type)) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "posterior"), "gray_posterior", tree_type)) %>%
  group_by(tree_type, Feature_ID) %>% 
  summarise(mean_D = mean(Destimate), 
            mean_Pval1 = mean(Pval1),
            mean_Pval0 = mean(Pval0), 
            ntip = mean(n), 
            zeroes = mean(zeroes), 
            ones = mean(ones), .groups = "drop") %>% 
  mutate(one_is_one = ifelse(ones == 1 |zeroes== 1, "yes", "no")) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  mutate(min = ifelse(ones < zeroes, ones, zeroes)) %>% 
  mutate(min_p = min / (ones + zeroes))

#reading in reconstruction results
parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 1, glottolog_parsimony_prediction_1, NA)) %>% 
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 0, glottolog_parsimony_prediction_0, glottolog_parsimony_prediction_number)) %>%   dplyr::select(Feature_ID, glottolog_parsimony_prediction_number) %>% 
  mutate(tree_type = "glottolog") %>% 
  tidyr::pivot_longer(cols = c("glottolog_parsimony_prediction_number"))

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_prediction_number)) %>%   dplyr::select(Feature_ID, gray_parsimony_prediction_number) %>% 
  mutate(tree_type = "gray_mcct") %>% 
  tidyr::pivot_longer(cols = c("gray_parsimony_prediction_number"))

parsimony_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_posteriors_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_posteriors_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_posteriors_prediction_number)) %>%   dplyr::select(Feature_ID, gray_parsimony_posteriors_prediction_number) %>% 
  mutate(tree_type = "gray_posterior")  %>% 
  tidyr::pivot_longer(cols = c("gray_parsimony_posteriors_prediction_number"))


ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 1, glottolog_ML_prediction_1, NA)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 0, glottolog_ML_prediction_0, glottolog_ML_prediction_number)) %>%
  dplyr::select(Feature_ID, glottolog_ML_prediction_number) %>% 
  mutate(tree_type = "glottolog") %>% 
  tidyr::pivot_longer(cols = c("glottolog_ML_prediction_number"))

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_ML_prediction_number)) %>%
  dplyr::select(Feature_ID, gray_ML_prediction_number) %>% 
  mutate(tree_type = "gray_mcct") %>% 
  tidyr::pivot_longer(cols = c("gray_ML_prediction_number"))


ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_posteriors_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_posteriors_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_posteriors_ML_prediction_number)) %>% 
  dplyr::select(Feature_ID, gray_posteriors_ML_prediction_number) %>% 
  mutate(tree_type = "gray_posterior") %>% 
  tidyr::pivot_longer(cols = c("gray_posteriors_ML_prediction_number"))

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(most_common_prediction_number = ifelse(Prediction == 1,`1` , NA)) %>% 
  mutate(most_common_prediction_number = ifelse(Prediction == 0, `0`, most_common_prediction_number)) %>% 
  dplyr::select(Feature_ID, most_common_prediction_number) %>% 
  mutate(tree_type = "most_common") %>% 
  tidyr::pivot_longer(cols = c("most_common_prediction_number"))
  
reconstruction_results_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimony_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_df)

#%>% 
#  mutate(result_points = ifelse(test = str_detect(string = reconstruction_result, pattern = "Half"), yes = 0.5, no = NA)) %>% 
#  mutate(result_points = ifelse(test = str_detect(string = reconstruction_result, pattern = "True"), yes = 1, no = result_points)) %>% 
  
#  mutate(result_points = ifelse(test = str_detect(string = reconstruction_result, pattern = "False"), yes = 0, no = result_points))


#joning and plottin

reconstruction_results_df %>% 
  left_join(phylo_d_df, by = c("Feature_ID", "tree_type")) %>% 
  filter(min > 1) %>% 
  ggplot() +
  geom_point(mapping = aes(y = mean_D, x = value)) +
  theme_minimal()

