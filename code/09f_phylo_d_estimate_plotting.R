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
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 0, glottolog_parsimony_prediction_0, glottolog_parsimony_prediction_number)) %>%   dplyr::select(Prediction, `Parsimony result (Glottolog-tree)`, Feature_ID, glottolog_parsimony_prediction_number, glottolog_parsimony_prediction_0, glottolog_parsimony_prediction_1) %>% 
  mutate(tree_type = "glottolog")

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_prediction_number)) %>%   dplyr::select(Prediction, `Parsimony result (Gray et al 2009-tree)`, Feature_ID, gray_parsimony_prediction_number, gray_parsimony_prediction_0, gray_parsimony_prediction_1) %>% 
  mutate(tree_type = "gray_mcct")

parsimony_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_prediction_number)) %>%   dplyr::select(Prediction, `Parsimony result (Gray et al 2009-tree posteriors)`, Feature_ID, gray_parsimony_prediction_number, gray_parsimony_prediction_0, gray_parsimony_prediction_1) %>% 
  mutate(tree_type = "gray_posterior")

ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 1, glottolog_ML_prediction_1, NA)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 0, glottolog_ML_prediction_0, glottolog_ML_prediction_number)) %>%   dplyr::select(Prediction, `ML result (Glottolog-tree)`, Feature_ID, glottolog_ML_prediction_number, glottolog_ML_prediction_0, glottolog_ML_prediction_1) %>% 
  mutate(tree_type = "glottolog")

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_ML_prediction_number)) %>%   dplyr::select(Prediction, `ML result (Gray et al 2009-tree)`, Feature_ID, gray_ML_prediction_number, gray_ML_prediction_0, gray_ML_prediction_1) %>% 
  mutate(tree_type = "gray_mcct")

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_ML_prediction_number)) %>% 
  dplyr::select(Prediction, `ML result (Gray et al 2009-tree posteriors)`, Feature_ID, gray_ML_prediction_number, gray_ML_prediction_0, gray_ML_prediction_1) %>% 
  mutate(tree_type = "gray_posterior")

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  dplyr::select(`result_most_common`, Feature_ID) %>% 
  mutate(tree_type = "glottolog")
  
reconstruction_results_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimony_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_df) %>% 
  filter(!is.na(reconstruction_result)) %>% 
  mutate(result_points = ifelse(test = str_detect(string = reconstruction_result, pattern = "Half"), yes = 0.5, no = NA)) %>% 
  mutate(result_points = ifelse(test = str_detect(string = reconstruction_result, pattern = "True"), yes = 1, no = result_points)) %>% 
  
  mutate(result_points = ifelse(test = str_detect(string = reconstruction_result, pattern = "False"), yes = 0, no = result_points))


#joning and plottin

reconstruction_results_df %>% 
  full_join(phylo_d_df) %>% 
  ggplot() +
  geom_point(mapping = aes(x = mean_D, y = result_points)) +
  facet_grid(~Feature_tree)


reconstruction_results_df %>% 
  full_join(phylo_d_df) %>% 
  group_by(result_points) %>% 
  summarise(mean_d = mean(mean_D, na.rm = T)) %>% View()
