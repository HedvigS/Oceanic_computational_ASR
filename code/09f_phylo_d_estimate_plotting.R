source("01_requirements.R")

fns <- list.files("output/HL_comparison/", pattern = "phylo_d.*tsv", full.names = T)

phylo_d_full <- fns %>% 
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
  mutate(one_is_one = ifelse(ones == 1 |zeroes== 1, "yes", "no")) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  mutate(min = ifelse(ones < zeroes, ones, zeroes)) %>% 
  mutate(min_p = min / (ones + zeroes))

phylo_d_df <- phylo_d_full %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  group_by(tree_type, Feature_ID, Feature_tree) %>% 
  summarise(mean_D = mean(Destimate), 
            mean_Pval1 = mean(Pval1),
            mean_Pval0 = mean(Pval0), 
            ntip = mean(n), 
            zeroes = mean(zeroes), 
            min = mean(min),
            min_p = mean(min_p),
            ones = mean(ones), .groups = "drop") 

#reading in reconstruction results
parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 1, glottolog_parsimony_prediction_1, NA)) %>% 
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 0, glottolog_parsimony_prediction_0, glottolog_parsimony_prediction_number)) %>%   dplyr::select(Feature_ID, glottolog_parsimony_prediction_number) %>% 
  mutate(tree_type = "glottolog") %>% 
  tidyr::pivot_longer(cols = c("glottolog_parsimony_prediction_number")) %>% 
  mutate(method = "parsimony") 
  

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_prediction_number)) %>%   dplyr::select(Feature_ID, gray_parsimony_prediction_number) %>% 
  mutate(tree_type = "gray_mcct") %>% 
  tidyr::pivot_longer(cols = c("gray_parsimony_prediction_number")) %>% 
  mutate(method = "parsimony") 

parsimony_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_posteriors_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_posteriors_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_posteriors_prediction_number)) %>%   dplyr::select(Feature_ID, gray_parsimony_posteriors_prediction_number) %>% 
  mutate(tree_type = "gray_posterior")  %>% 
  tidyr::pivot_longer(cols = c("gray_parsimony_posteriors_prediction_number")) %>% 
  mutate(method = "parsimony") 


ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 1, glottolog_ML_prediction_1, NA)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 0, glottolog_ML_prediction_0, glottolog_ML_prediction_number)) %>%
  dplyr::select(Feature_ID, glottolog_ML_prediction_number) %>% 
  mutate(tree_type = "glottolog") %>% 
  tidyr::pivot_longer(cols = c("glottolog_ML_prediction_number")) %>% 
  mutate(method = "ML") 

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_ML_prediction_number)) %>%
  dplyr::select(Feature_ID, gray_ML_prediction_number) %>% 
  mutate(tree_type = "gray_mcct") %>% 
  tidyr::pivot_longer(cols = c("gray_ML_prediction_number"))%>% 
  mutate(method = "ML") 


ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_posteriors_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_posteriors_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_posteriors_ML_prediction_number)) %>% 
  dplyr::select(Feature_ID, gray_posteriors_ML_prediction_number) %>% 
  mutate(tree_type = "gray_posterior") %>% 
  tidyr::pivot_longer(cols = c("gray_posteriors_ML_prediction_number"))%>% 
  mutate(method = "ML") 

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(most_common_prediction_number = ifelse(Prediction == 1,`1` , NA)) %>% 
  mutate(most_common_prediction_number = ifelse(Prediction == 0, `0`, most_common_prediction_number)) %>% 
  dplyr::select(Feature_ID, most_common_prediction_number) %>% 
  mutate(tree_type = "most_common") %>% 
  tidyr::pivot_longer(cols = c("most_common_prediction_number")) %>% 
  mutate(method = "most_common") 
  
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
  filter(min_p > 0.01) %>% 
  ggplot(mapping = aes(y = mean_D, x = value, color = name)) +
  geom_point() +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "black",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  theme_bw() +
  scale_x_continuous(expand=c(0.01,0.01)) +
  theme(legend.position = 0, 
        strip.text = element_text(face = "bold", colour = "black"),
          strip.background = element_rect(fill = "whitesmoke")) +
  facet_grid(method ~ tree_type) +
  xlab("Concurrence with HL") +
  ylab("D-estimate")
             

phylo_d_df$Feature_ID <- fct_reorder(phylo_d_df$Feature_ID, phylo_d_df$mean_D)

phylo_d_df %>% 
  filter(min > 1) %>% 
#  .[1:30,] %>% 
  filter(tree_type == "glottolog") %>% 
  mutate(Pval0_sig = ifelse(mean_Pval0 > 0.05 & mean_D < 1, "yes", "no"),
         Pval1_sig = ifelse(mean_Pval1 > 0.05& mean_D > 0, "yes", "no")) %>% 
  ggplot(aes(y = Feature_ID, x = mean_D)) +
#  geom_point(mapping = aes(y = Feature_ID, x = mean_D, fill =  Pval0_sig), shape = 21)
  geom_text(label = "\u25D7",  mapping= aes(color = as.character(Pval1_sig)),  
            size=10, family = "Arial Unicode MS") +
  geom_text(label = "\u25D6", mapping= aes(color = as.character(Pval0_sig)), 
            size=10, family = "Arial Unicode MS") 
  


cap <- "Table showing D-estimate (phylogenetic signal) of Grambank features."
lbl <- "d_estimate_summary"
align <- c("r", "l","l","l") 

library(magrittr)

phylo_d_full %>% 
  filter(min > 1) %>%
  mutate(Pval0_sig = ifelse(Pval0 > 0.05 & Destimate < 1, "yes", "no")) %>% View()
  group_by(tree_type) %>% 
  mutate(mean_D = mean(Destimate)) %>% 
  group_by(tree_type, Pval0_sig) %>% 
  summarise(n = n(),
            mean_D = first(mean_D),
            .groups = "drop") %>% 
  group_by(tree_type) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(prop = n/sum) %>% 
  filter(Pval0_sig == "yes") %>% 
  dplyr::select(tree = tree_type, `D-estimate (mean)` = mean_D, `Propotion of features signficantly similar to 0` = prop) %>% 
  write_tsv("output/D_estimate_summary.tsv", na = "") %T>%
  xtable(caption = cap, label = lbl,
         align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "D-estimate_summary.tex"),
                       include.rownames = FALSE) 
         