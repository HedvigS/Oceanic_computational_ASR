source("01_requirements.R")

HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") 

phylo_d_df_full <-  read_tsv("output/HL_comparison/phylo_d//phylo_d_full.tsv") %>% 
  inner_join(HL_findings_sheet, by = "Feature_ID", relationship = "many-to-many") 


phylo_d_df <-  read_tsv("output/HL_comparison/phylo_d/phylo_d_df.tsv") %>% 
  inner_join(HL_findings_sheet, by = "Feature_ID", relationship = "many-to-many") 

posteriors <- list.files("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", pattern = "*.txt") %>% length()

#df of features that were exlcuded because too few tips
phylo_d_df_missing <- phylo_d_df_full %>%  
  mutate(missing = ifelse(is.na(Destimate), 1, 0)) %>% 
  group_by(tree_type) %>% 
  summarise(`Too few tips altogether` = sum(missing))  %>% 
  mutate(`Too few tips altogether` = ifelse(str_detect(tree_type, "poster"),`Too few tips altogether`/posteriors, `Too few tips altogether`))  
  
phylo_d_df <-  phylo_d_df %>% 
    filter(!is.na(mean_D))

#reading in reconstruction results
reconstruction_results_df <- read_tsv("output/all_reconstructions_all_methods_long.tsv", col_types = cols(.default = "c")) %>% 
  filter(!is.na(value)) %>% 
  filter(is.na(conflict)) %>% 
  filter(variable == "prediction_1"|
           variable == "prediction_0") %>% 
  pivot_wider(id_cols = c("Feature_ID", "Proto-language", "tree_type", "method"), names_from = "variable", values_from = "value") %>% 
  left_join(HL_findings_sheet, by = c("Feature_ID", "Proto-language")  ) %>% 
  mutate(value = ifelse(Prediction == 1,`prediction_1` , NA)) %>% 
  mutate(value = ifelse(Prediction == 0, `prediction_0`, value)) %>% 
  unite(Feature_ID, tree_type, col = "Feature_tree", remove = F) %>% 
  mutate(value = as.numeric(value))

#joning and plotting

joined_df <- reconstruction_results_df %>% 
  inner_join(phylo_d_df, by = c("Feature_tree", "Feature_ID", "Proto-language", "tree_type", "Prediction")) %>% 
  mutate(tree_type = str_replace(tree_type, "_", " - ")) %>% 
  mutate(tree_type = str_replace(tree_type, "gray", "Gray (2009)")) %>% 
  distinct(Feature_tree, mean_D, summarise_col, method, `Proto-language`, .keep_all = T)


#plot of percentage of minority state per tree and kind of d-estimate
joined_df %>%
  filter(!str_detect(tree_type, "common")) %>%
  distinct(Feature_tree, tree_type, summarise_col, min_p) %>% 
  ggplot() +
  geom_point(mapping = aes(x = tree_type, y = min_p)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~summarise_col)

joined_df %>% 
  filter(tree_type != "most_common") %>% 
 filter(summarise_col != "similar to 0") %>%
 filter(summarise_col != "similar to both, below 0") %>%
filter(summarise_col != "similar to both, above 1") %>%
 filter(summarise_col != "similar to both, between 0 & 1")  %>%
 filter(!is.na(value)) %>% 
 filter(mean_D < 5) %>%
 filter(mean_D > -5) %>%
 ggplot(mapping = aes(x = mean_D, y = value)) +
 geom_point(mapping = aes(color = summarise_col)) +
 ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                  label.y.npc="bottom", label.x.npc = "left", alpha = 0.8)


#plot of correlation between d-estimate and agreement with HL, excluding d-estimates that are inappropriate
joined_df %>% 
  filter(tree_type != "most_common") %>% 
  filter(summarise_col != "all same"|
           summarise_col != "similar to both, above 1"|
           summarise_col != "similar to both, below 0",
         summarise_col != "singleton")  %>%
  filter(!is.na(value)) %>% 
  ggplot(mapping = aes(x = mean_D, y = value)) +
  geom_point(mapping = aes(color = summarise_col)) +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="bottom", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm', formula = 'y ~ x') +
  facet_grid(tree_type~method) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  xlab ("D-estimate (mean)") +
  ylab("Concurrance with HL")

ggsave(filename = paste0(OUTPUTDIR_plots, "phylo_d_vs_HL_concurrance.png"), width = 10, height = 9)

###############
#latex tables##
###############

table_P_values_summarised <- phylo_d_df_full %>% 
  filter(!str_detect(tree_type, "common")) %>%
  group_by(tree_type, summarise_col) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  complete(tree_type, summarise_col, fill = list(n = 0)) %>% 
  mutate(n = ifelse(str_detect(tree_type, "poster"),n/posteriors, n))  %>% 
  reshape2::dcast(tree_type ~ summarise_col, value.var = "n")  
  

table_P_values_summarised_latex_green <- table_P_values_summarised %>% 
  dplyr::select(tree = tree_type, 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright similar to 0}}}$"= "similar to 0", 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright similar to both, between 0 \\& 1}}}$" = "similar to both, between 0 & 1", 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright similar to 1}}}$"=  "similar to 1", 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright dissimilar to both, between 0 \\& 1}}}$"=  "dissimilar to both, between 0 & 1")

cap <- "Table of types of D-estimates per tree, data-points included."
lbl <- "phylo_d_summarise_col_green"
align <- c("r","p{3cm}", "p{3cm}", "p{3cm}","p{3cm}","p{3cm}") 

table_P_values_summarised_latex_green  %>% 
  xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "table_P_values_summarised_latex_green.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, 
                       math.style.negative = F,
                       table.placement = "ht",
                       booktabs = TRUE, hline.after = c(-1, 0, nrow(table_P_values_summarised_latex_green))) 



table_P_values_summarised_latex_orange <- table_P_values_summarised %>% 
  dplyr::select(tree = tree_type, 
                #   "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright all same}}}$"= "all same",
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright singleton}}}$"= "singleton",
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright similar to both, above 1}}}$"= "similar to both, above 1",
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright similar to both, below 0}}}$"= "similar to both, below 0")

orange_for_summary <- table_P_values_summarised_latex_orange %>% 
  reshape2::melt(id.vars = "tree") %>% 
  group_by(tree) %>% 
  summarise(`features unfit for D-estimate` = sum(value)) %>% 
  rename(tree_type = tree)


cap <- "Table of types of D-estimates per tree, data-points not included."
lbl <- "phylo_d_summarise_col_orange"
align <- c("r","p{3cm}", "p{3cm}", "p{3cm}","p{3cm} ") 


table_P_values_summarised_latex_orange  %>% 
  xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "table_P_values_summarised_latex_orange.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, 
                       math.style.negative = F,
                       table.placement = "ht",
                       booktabs = TRUE, hline.after = c(-1, 0, nrow(table_P_values_summarised_latex_orange))) 

#summary
phylo_d_summarised_table_pval0sig <-  phylo_d_df_full %>% 
  filter(!str_detect(tree_type, "common")) %>%
  filter(summarise_col != "all same") %>% 
  filter(summarise_col != "similar to both, above 1") %>% 
  filter(summarise_col != "similar to both, below 0") %>% 
  filter(summarise_col != "singleton")  %>%
  mutate(Pval0_sig = ifelse(Pval0 > 0.05 & Destimate < 1, "similar to 0", "dissimilar to 0")) %>% 
  group_by(tree_type, Pval0_sig) %>%
  summarise(n = n(), .groups = "drop") %>% 
  mutate(n = ifelse(str_detect(tree_type, "poster"),n/posteriors, n))  %>% 
  group_by(tree_type) %>%
  mutate(sum = sum(n)) %>%
  mutate(prop = n/sum) %>% 
  filter(Pval0_sig == "similar to 0") %>% 
  ungroup() %>% 
  mutate(prop = paste0(round(x = prop,  digits = 2)*100, "%")) %>%
  dplyr::select(tree_type, `Proportion of features significantly similar to 0` = prop)

phylo_d_summarised_table <- phylo_d_df_full %>%   
  filter(!str_detect(tree_type, "common")) %>%
  filter(summarise_col != "all same") %>% 
  filter(summarise_col != "similar to both, above 1") %>% 
  filter(summarise_col != "similar to both, below 0") %>% 
  filter(summarise_col != "singleton")  %>%
  group_by(tree_type) %>% 
  summarise(mean_D = mean(Destimate) %>% round(2)) %>%
  full_join(phylo_d_summarised_table_pval0sig, by = "tree_type") %>% 
  full_join(orange_for_summary, by = "tree_type") %>% 
  full_join(  phylo_d_df_missing, by = "tree_type") %>% 
  dplyr::select(tree  = tree_type, `D-estimate (mean)` = mean_D, `Proportion of features significantly similar to 0`, `features unfit for D-estimate`, `Too few tips altogether`)

phylo_d_summarised_table %>% 
  write_tsv("output/D_estimate_summary.tsv", na = "")

cap <- "Table showing D-estimate (phylogenetic signal) of Grambank features that map onto research in traditional historical linguistics."
lbl <- "d_estimate_summary"
align <- c("r", "p{6cm}","p{2.2cm}","p{2.2cm}","p{2.2cm}", "p{2.2cm}") 


phylo_d_summarised_table %>% 
  mutate(tree = str_replace(tree, "_", " - ")) %>%
  mutate(tree = str_replace(tree, "glottolog", "Glottolog")) %>% 
  mutate(tree = str_replace(tree, "gray", "Gray (2009)")) %>% 
  xtable(caption = cap, label = lbl,
         align = align) %>%
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "D-estimate_summary.tex"),
                       include.rownames = FALSE)
