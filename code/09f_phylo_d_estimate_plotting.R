source("01_requirements.R")

HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") %>% 
  distinct(Feature_ID)

fns <- list.files("output/HL_comparison/phylo_d/", pattern = "main", full.names = T)

phylo_d_full <- fns %>% 
  map_df(
    function(x) qs::qread(x)#data.table::fread(x ,
       #                           encoding = 'UTF-8', header = TRUE, 
      #                            fill = TRUE, blank.lines.skip = TRUE,
     #                             sep = "\t", na.strings = "",
    #) 
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
            n = n(),
            zeroes = mean(zeroes), 
            min = mean(min),
            min_p = mean(min_p),
            ones = mean(ones), .groups = "drop") 

#reading in reconstruction results
parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 1, glottolog_parsimony_prediction_1, NA)) %>% 
  mutate(glottolog_parsimony_prediction_number = ifelse(Prediction == 0, glottolog_parsimony_prediction_0, glottolog_parsimony_prediction_number)) %>%   dplyr::select(Feature_ID, glottolog_parsimony_prediction_number, `Proto-language`) %>% 
  mutate(tree_type = "glottolog") %>% 
  tidyr::pivot_longer(cols = c("glottolog_parsimony_prediction_number")) %>% 
  mutate(method = "parsimony") 
  

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_prediction_number)) %>%   dplyr::select(Feature_ID, gray_parsimony_prediction_number, `Proto-language`) %>% 
  mutate(tree_type = "gray_mcct") %>% 
  tidyr::pivot_longer(cols = c("gray_parsimony_prediction_number")) %>% 
  mutate(method = "parsimony") 

parsimony_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_parsimony_posteriors_prediction_number = ifelse(Prediction == 1, gray_parsimony_prediction_1, NA)) %>% 
  mutate(gray_parsimony_posteriors_prediction_number = ifelse(Prediction == 0, gray_parsimony_prediction_0, gray_parsimony_posteriors_prediction_number)) %>%   dplyr::select(Feature_ID, gray_parsimony_posteriors_prediction_number, `Proto-language`) %>% 
  mutate(tree_type = "gray_posterior")  %>% 
  tidyr::pivot_longer(cols = c("gray_parsimony_posteriors_prediction_number")) %>% 
  mutate(method = "parsimony") 


ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 1, glottolog_ML_prediction_1, NA)) %>% 
  mutate(glottolog_ML_prediction_number = ifelse(Prediction == 0, glottolog_ML_prediction_0, glottolog_ML_prediction_number)) %>%
  dplyr::select(Feature_ID, glottolog_ML_prediction_number, `Proto-language`) %>% 
  mutate(tree_type = "glottolog") %>% 
  tidyr::pivot_longer(cols = c("glottolog_ML_prediction_number")) %>% 
  mutate(method = "ML") 

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_ML_prediction_number)) %>%
  dplyr::select(Feature_ID, gray_ML_prediction_number, `Proto-language`) %>% 
  mutate(tree_type = "gray_mcct") %>% 
  tidyr::pivot_longer(cols = c("gray_ML_prediction_number"))%>% 
  mutate(method = "ML") 


ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(gray_posteriors_ML_prediction_number = ifelse(Prediction == 1, gray_ML_prediction_1, NA)) %>% 
  mutate(gray_posteriors_ML_prediction_number = ifelse(Prediction == 0, gray_ML_prediction_0, gray_posteriors_ML_prediction_number)) %>% 
  dplyr::select(Feature_ID, gray_posteriors_ML_prediction_number, `Proto-language`) %>% 
  mutate(tree_type = "gray_posterior") %>% 
  tidyr::pivot_longer(cols = c("gray_posteriors_ML_prediction_number"))%>% 
  mutate(method = "ML") 

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  mutate(most_common_prediction_number = ifelse(Prediction == 1,`1` , NA)) %>% 
  mutate(most_common_prediction_number = ifelse(Prediction == 0, `0`, most_common_prediction_number)) %>% 
  dplyr::select(Feature_ID, most_common_prediction_number, "Proto-language", min, ntip) %>% 
  mutate(tree_type = "most_common") %>% 
  tidyr::pivot_longer(cols = c("most_common_prediction_number")) %>% 
  mutate(method = "most_common") 
  
reconstruction_results_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df, by = c("Feature_ID", "tree_type", "name", "value", "method", "Proto-language")) %>% 
  full_join(parsimony_gray_posteriors_df, by = c("Feature_ID", "tree_type", "name", "value", "method", "Proto-language")) %>% 
  full_join(ML_glottolog_df, by = c("Feature_ID", "tree_type", "name", "value", "method", "Proto-language")) %>% 
  full_join(ML_gray_mcct, by = c("Feature_ID", "tree_type", "name", "value", "method", "Proto-language")) %>% 
  full_join(ML_gray_posteriors_df, by = c("Feature_ID", "tree_type", "name", "value", "method", "Proto-language")) %>% 
  full_join(most_common_df, by = c("Feature_ID", "tree_type", "name", "value", "method", "Proto-language"))


#joning and plotting

phylo_d_df <- phylo_d_df %>% 
  left_join(reconstruction_results_df) %>% 
  dplyr::select(mean_D, mean_Pval1, mean_Pval0, Feature_ID,  tree_type, ntip, min) %>% 
  mutate(summarise_col = ifelse(mean_Pval0 > 0.05 &
                                   mean_Pval1 > 0.05 & 
                                   mean_D < 0, 
                                 "similar to both, below 0", NA)) %>%   
mutate(summarise_col = if_else(mean_Pval0 > 0.05 &
                                 mean_Pval1 > 0.05 & 
                                 mean_D > 1, 
                               "similar to both, above 1", summarise_col))   %>% 
mutate(summarise_col = if_else(mean_Pval0 > 0.05 &
                                 mean_Pval1 > 0.05 & 
                                 between(mean_D, lower = 0, upper = 1), 
                               "similar to both, between 0 & 1", summarise_col))   %>% 
  mutate(summarise_col = if_else(mean_Pval0 > 0.05 &
                                   mean_Pval1 < 0.05, 
                                 "similar to 0", summarise_col))   %>% 
  mutate(summarise_col = if_else(mean_Pval0 < 0.05 &
                                   mean_Pval1 > 0.05, 
                                 "similar to 1", summarise_col))  %>% 
  mutate(summarise_col = if_else(mean_Pval0 < 0.05 &
                                   mean_Pval1 < 0.05 & 
                                   between(mean_D, lower = 0, upper = 1), 
                                 "dissimilar to both, between 0 & 1", summarise_col)) %>% 
  
  mutate(summarise_col = ifelse(min == 0, "all same", summarise_col)) %>%   
  mutate(summarise_col = if_else(min == 1, "one off", summarise_col))
  


phylo_d_df %>%
  distinct(Feature_ID, tree_type, mean_D, summarise_col, mean_Pval0, mean_Pval1, ntip, min) %>% 
#  filter(tree_type != "most_common") %>% 
#  group_by(summarise_col, tree_type) %>% 
#  summarise(n = n(), .groups = "drop") %>% 
  ggplot() +
  geom_point(mapping = aes(x = tree_type , y = min), stat = "identity")+
  facet_wrap(~summarise_col)

joined_df <- reconstruction_results_df %>% 
  left_join(phylo_d_df) 


joined_df %>% View()
  filter(tree_type != "most_common") %>%
#  filter(summarise_col == "similar to 1"|
#           summarise_col == "similar to 0"|
#           summarise_col == "similar to both, between 0 & 1"|
#           summarise_col == "dissimilar to both, between 0 & 1")  %>% 
  ggplot() +
  geom_point(mapping = aes(x = min, y = value)) +
  facet_grid(tree_type~method)


joined_df$summarise_col %>% unique()




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
  


cap <- "Table showing D-estimate (phylogenetic signal) of Grambank features that map onto research in traditional historical linguistics."
lbl <- "d_estimate_summary"
align <- c("r", "l","l","l") 


phylo_d_summarised_table <- phylo_d_full %>% 
  filter(min > 1) %>% 
  inner_join(HL_findings_sheet, by = "Feature_ID") %>% 
  mutate(Pval0_sig = ifelse(Pval0 > 0.05 & Destimate < 1, "yes", "no")) %>%
  group_by(tree_type) %>% 
  mutate(mean_D = mean(Destimate)) %>% 
  group_by(tree_type, Pval0_sig) %>% 
  summarise(n = n(),
            mean_D = first(mean_D),
            .groups = "drop") %>% 
  group_by(tree_type) %>% 
  mutate(sum = sum(n)) %>% 
  mutate(prop = n/sum) %>%
  mutate(mean_D = round(mean_D, 2)) %>% 
    filter(Pval0_sig == "yes") %>%
  mutate(prop = paste0(round(prop, 2)*100, "%")) %>% 
  dplyr::select(tree = tree_type, `D-estimate (mean)` = mean_D, `Proportion of features signficantly similar to 0` = prop)

phylo_d_summarised_table$tree <- phylo_d_summarised_table$tree %>% 
  str_replace("glottolog", "Glottolog") %>% 
  str_replace("gray_mcct", "Gray et al 2009-MCCT") %>% 
  str_replace("gray_posterior", "Gray et al 2009-posteriors") 

phylo_d_summarised_table %>% 
  write_tsv("output/D_estimate_summary.tsv", na = "") 

phylo_d_summarised_table %>% 
  xtable(caption = cap, label = lbl,
         align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "D-estimate_summary.tex"),
                       include.rownames = FALSE) 
         
