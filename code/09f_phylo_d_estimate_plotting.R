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
            ones = mean(ones)) %>% 
  mutate(one_is_one = ifelse(ones == 1 |zeroes== 1, "yes", "no")) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  mutate(min = ifelse(ones < zeroes, ones, zeroes)) %>% 
  mutate(min_p = min / (ones + zeroes))


#reading in reconstruction results

parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  dplyr::select(reconstruction_result = `Parsimony result (Glottolog-tree)`, Feature_ID)  %>% 
  mutate(tree_type = "glottolog")

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  dplyr::select(reconstruction_result = `Parsimony result (Gray et al 2009-tree)`, Feature_ID) %>% 
  mutate(tree_type = "gray_mcct")

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  dplyr::select(reconstruction_result = `Parsimony result (Gray et al 2009-tree posteriors)`, Feature_ID)   %>% 
  mutate(tree_type = "gray_posterior")

ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  dplyr::select(reconstruction_result = `ML result (Glottolog-tree)`, Feature_ID) %>% 
  mutate(tree_type = "glottolog")

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  dplyr::select(reconstruction_result = `ML result (Gray et al 2009-tree)`, Feature_ID) %>% 
  mutate(tree_type = "gray_mcct")

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  dplyr::select(reconstruction_result = `ML result (Gray et al 2009-tree posteriors)`, Feature_ID) %>% 
  mutate(tree_type = "gray_posterior")

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  dplyr::select(reconstruction_result = `result_most_common`, Feature_ID) %>% 
  mutate(tree_type = "glottolog")
  
reconstruction_results_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df, by = c("reconstruction_result", "Feature_ID", "tree_type")) %>% 
  full_join(parsimon_gray_posteriors_df, by = c("reconstruction_result", "Feature_ID", "tree_type")) %>% 
  full_join(ML_glottolog_df, by = c("reconstruction_result", "Feature_ID", "tree_type")) %>% 
  full_join(ML_gray_mcct, by = c("reconstruction_result", "Feature_ID", "tree_type")) %>% 
  full_join(ML_gray_posteriors_df, by = c("reconstruction_result", "Feature_ID", "tree_type")) %>% 
  full_join(most_common_df, by = c("reconstruction_result", "Feature_ID", "tree_type")) %>% 
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
