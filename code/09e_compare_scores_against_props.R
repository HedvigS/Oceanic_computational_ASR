source("01_requirements.R")

HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") 

phylo_d_df <-  read_tsv("output/HL_comparison/phylo_d/phylo_d_df.tsv") %>% 
  filter(summarise_col == "similar to 1"|
           summarise_col == "similar to 0"|
           summarise_col == "similar to both, between 0 & 1"|
           summarise_col == "dissimilar to both, between 0 & 1")  
  
#reading in reconstruction results
reconstruction_results_df_full <- read_tsv("output/all_reconstructions_all_methods_long.tsv", col_types = cols(.default = "c")) %>% 
  filter(!is.na(value)) %>% 
  filter(is.na(conflict))

#reading in reconstruction results
reconstruction_results_df <- reconstruction_results_df_full %>% 
  filter(variable == "prediction_1"|
           variable == "prediction_0") %>% 
  pivot_wider(id_cols = c("Feature_ID", "Proto-language", "tree_type", "method"), names_from = "variable", values_from = "value") %>% 
  left_join(HL_findings_sheet, by = c("Feature_ID", "Proto-language")  ) %>% 
  mutate(value = ifelse(Prediction == 1,`prediction_1` , NA)) %>% 
  mutate(value = ifelse(Prediction == 0, `prediction_0`, value)) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  dplyr::select("Feature_ID", "Proto-language", "tree_type", "method", HL_agreement = value) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) 

min_p_df <- reconstruction_results_df_full %>% 
  filter(variable == "min_percent") %>% 
  pivot_wider(id_cols = c("Feature_ID", "Proto-language", "tree_type", "method"), names_from = "variable", values_from = "value")  %>% 
  mutate(min_percent = as.numeric(min_percent)) %>% 
  dplyr::select("Feature_ID", "Proto-language", "tree_type", "method", min_percent)

joined <- reconstruction_results_df %>% 
  full_join(min_p_df, by = c("Feature_ID", "Proto-language", "tree_type", "method")) %>% 
  left_join(phylo_d_df, by = c("Feature_tree", "Feature_ID", "tree_type")) %>% 
  mutate(tree_type = str_replace(tree_type, "_", " - ")) %>% 
  mutate(tree_type = str_replace(tree_type, "gray", "Gray (2009)"))

joined %>% 
  filter(!is.na(HL_agreement)) %>% 
  filter(!is.na(min_percent)) %>% 
  filter(!str_detect(tree_type, "common")) %>% 
  ggplot(mapping = aes(x = min_percent, y = HL_agreement)) +
  geom_point(mapping = aes(color = tree_type)) +
  ggpubr::stat_cor(method = "pearson", p.digits = 3, geom = "label", color = "blue",
                   label.y.npc="bottom", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  facet_grid(tree_type~method) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  scale_y_continuous(labels = scales::percent, limits = c(-0,1.1), breaks = c(0,0.25, 0.5, 0.75, 1)) +
  theme(legend.position =  0) +
  scale_color_manual(values= wes_palette("Darjeeling1", n = 3)) +
  xlab ("Percentage of minority state") +
  ylab("Concurrance with HL")

ggsave(filename = paste0(OUTPUTDIR_plots, "min_p_vs_HL_concurrance.png"), width = 7 , height = 9)
