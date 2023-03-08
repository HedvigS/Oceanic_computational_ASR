source("01_requirements.R")

HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") 

fns <- list.files("output/HL_comparison/phylo_d_old/", pattern = "main", full.names = T)

phylo_d_full <- fns %>% 
  map_df(
    function(x) qs::qread(x)) %>% 
  distinct() %>% 
  rename(Feature_ID = Feature) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "ct"), "gray_mcct", "other")) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "glottolog"), "glottolog", tree_type)) %>% 
  mutate(tree_type =ifelse(str_detect(tree, "posterior"), "gray_posteriors", tree_type)) %>% 
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
            Parameters_observed = mean(Parameters_observed),
            Parameters_MeanRandom = mean(Parameters_MeanRandom),
            Parameters_MeanBrownian = mean(Parameters_MeanBrownian),
            zeroes = mean(zeroes), 
            min = mean(min),
            min_p = mean(min_p),
            ones = mean(ones), .groups = "drop") %>% 
  dplyr::select(mean_D, mean_Pval1, mean_Pval0, Feature_ID,  n, tree_type, ntip, min, min_p, Feature_tree, Parameters_observed, Parameters_MeanRandom, Parameters_MeanBrownian) %>% 
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
  mutate(summarise_col = if_else(min == 1, "singleton", summarise_col))

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
  left_join(phylo_d_df, by = c("Feature_tree", "Feature_ID", "tree_type")) %>% 
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

#plot of percentage of minority state and difference between mean of sum clade differences in brownian and random
#joined_df %>% 
#  filter(!str_detect(tree_type, "common")) %>%
#  mutate(deff = abs(Parameters_MeanRandom - Parameters_MeanBrownian)) %>% 
#  ggplot()+
#  geom_point(mapping = aes(x = min_p, y = deff, color = tree_type, size = ntip))

#plot of correlation between d-estimate and agreement with HL, excluding d-estimates that are inappropriate
joined_df %>% 
  filter(tree_type != "most_common") %>% 
    filter(summarise_col == "similar to 1"|
             summarise_col == "similar to 0"|
             summarise_col == "similar to both, between 0 & 1"|
             summarise_col == "dissimilar to both, between 0 & 1")  %>% 
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

table_P_values_summarised <- joined_df %>% 
  filter(!str_detect(tree_type, "common")) %>%
  distinct(Feature_tree, tree_type, summarise_col) %>% 
  group_by(tree_type, summarise_col) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  complete(tree_type, summarise_col, fill = list(n = 0)) %>% 
  reshape2::dcast(tree_type ~ summarise_col, value.var = "n") 

table_P_values_summarised_latex_green <- table_P_values_summarised %>% 
  dplyr::select(tree = tree_type, 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright similar to 0}}}$"= "similar to 0", 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright similar to both, between 0 \\& 1}}}$" = "similar to both, between 0 & 1", 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright similar to 1}}}$"=  "similar to 1", 
                "$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright dissimilar to both, between 0 \\& 1}}}$"=  "dissimilar to both, between 0 & 1")

cap <- "Table of types of D-estimates per tree, data-points included."
lbl <- "phylo_d_summarise_col_green"
align <- c("r","p{3cm}", "p{3cm}", "p{3cm}","p{3cm} ","p{3cm}" ) 


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
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright all same}}}$"= "all same",
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright singleton}}}$"= "singleton",
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright similar to both, above 1}}}$"= "similar to both, above 1",
                "$\\textbf{\\cellcolor{spec_color_orange!50}{\\parbox{2.7cm}{\\raggedright similar to both, below 0}}}$"= "similar to both, below 0")


cap <- "Table of types of D-estimates per tree, data-points not included."
lbl <- "phylo_d_summarise_col_orange"
align <- c("r","p{3cm}", "p{3cm}", "p{3cm}","p{3cm} ","p{3cm}" ) 


table_P_values_summarised_latex_orange  %>% 
  xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "table_P_values_summarised_latex_orange.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, 
                       math.style.negative = F,
                       table.placement = "ht",
                       booktabs = TRUE, hline.after = c(-1, 0, nrow(table_P_values_summarised_latex_orange))) 

#              
# 
# phylo_d_df$Feature_ID <- fct_reorder(phylo_d_df$Feature_ID, phylo_d_df$mean_D)
# 
# phylo_d_df %>% 
#   filter(min > 1) %>% 
# #  .[1:30,] %>% 
#   filter(tree_type == "glottolog") %>% 
#   mutate(Pval0_sig = ifelse(mean_Pval0 > 0.05 & mean_D < 1, "yes", "no"),
#          Pval1_sig = ifelse(mean_Pval1 > 0.05& mean_D > 0, "yes", "no")) %>% 
#   ggplot(aes(y = Feature_ID, x = mean_D)) +
# #  geom_point(mapping = aes(y = Feature_ID, x = mean_D, fill =  Pval0_sig), shape = 21)
#   geom_text(label = "\u25D7",  mapping= aes(color = as.character(Pval1_sig)),  
#             size=10, family = "Arial Unicode MS") +
#   geom_text(label = "\u25D6", mapping= aes(color = as.character(Pval0_sig)), 
#             size=10, family = "Arial Unicode MS") 
#   
# 
# 
# cap <- "Table showing D-estimate (phylogenetic signal) of Grambank features that map onto research in traditional historical linguistics."
# lbl <- "d_estimate_summary"
# align <- c("r", "l","l","l") 
# 

phylo_d_summarised_table <-  joined_df %>%
  filter(!is.na(value)) %>% 
  distinct(Feature_tree, tree_type, summarise_col, mean_D, mean_Pval0, mean_Pval1) %>% 
  filter(!str_detect(tree_type, "common")) %>%
  filter(summarise_col == "similar to 1"|
           summarise_col == "similar to 0"|
           summarise_col == "similar to both, between 0 & 1"|
           summarise_col == "dissimilar to both, between 0 & 1")  %>%
  mutate(Pval0_sig = ifelse(mean_Pval0 > 0.05 & mean_D < 1, "yes", "no")) %>%
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

if(all(phylo_d_summarised_table$tree == table_P_values_summarised_latex_orange$tree)) {
  
phylo_d_summarised_table$`Feautres excluded due to too few tips in minority state` <- table_P_values_summarised_latex_orange[,2] +  table_P_values_summarised_latex_orange[,3] + table_P_values_summarised_latex_orange[,4] + table_P_values_summarised_latex_orange[,5]

}

phylo_d_summarised_table %>%
  write_tsv("output/D_estimate_summary.tsv", na = "")

 cap <- "Table showing D-estimate (phylogenetic signal) of Grambank features that map onto research in traditional historical linguistics."
 lbl <- "d_estimate_summary"
 align <- c("r", "p{5cm}","p{3cm}","p{3.5cm}", "p{3.5cm}") 

 
phylo_d_summarised_table %>%
  xtable(caption = cap, label = lbl,
         align = align) %>%
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "D-estimate_summary.tex"),
                       include.rownames = FALSE)

# 
# 
# 
# # # 
# # # ##trying to debug the algo  
# # # 
#  phylo_d_df$summarise_col %>% unique()
# # # 
#  phylo_d_df %>% 
#    filter(summarise_col ==  "dissimilar to both, between 0 & 1") %>%
#   dplyr::select(Feature_tree, summarise_col, Parameters_observed, Parameters_MeanRandom, Parameters_MeanBrownian) %>%
#   reshape2::melt(id.vars = c("summarise_col", "Feature_tree")) %>%
# #  reshape2::melt(id.vars = c("Feature_tree")) %>%
#     ggplot() +
#   geom_point(aes(x = value, y = Feature_tree, color = variable))# +
#   facet_wrap(~summarise_col)
# # 
# # 
# # nodalvals <- qs::qread("output/HL_comparison/phylo_d/phylo_d_table_GB314_glottolog_tree_newick_GB_pruned.txt_Nodalvals.qs")
# # 
# # main <- qs::qread("output/HL_comparison/phylo_d/phylo_d_table_GB314_glottolog_tree_newick_GB_pruned.txt_main.qs")
# # 
# # permutations <- qs::qread("output/HL_comparison/phylo_d/phylo_d_table_GB314_glottolog_tree_newick_GB_pruned.txt_permutations.qs")
# # 
# # phylo_d_df %>%
# #   distinct(Feature_ID, tree_type, mean_D, summarise_col, mean_Pval0, mean_Pval1, ntip, min) %>% 
# # #  filter(tree_type != "most_common") %>% 
# # #  group_by(summarise_col, tree_type) %>% 
# # #  summarise(n = n(), .groups = "drop") %>% 
# #   ggplot() +
# #   geom_point(mapping = aes(x = tree_type , y = min), stat = "identity")+
# #   facet_wrap(~summarise_col)
