source("01_requirements.R")
h_load("wesanderson")

full_df <- read_tsv("output/all_reconstructions_all_methods_long.tsv", col_types = cols(.default = "c")) %>%
  filter(variable == "result") %>% 
  filter(!is.na(value)) %>% 
  filter(is.na(conflict)) %>% 
  group_by(tree_type, method, value) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  unite(method, tree_type, remove = T, col = "Method", sep = " ") %>% 
  reshape2::dcast(Method ~ value, value.var = "n") %>% 
  mutate(`False Positive` = ifelse(Method =="most_common most_common" & is.na(`False Positive`), 0 ,`False Positive`)) # there are no False Positive for most common, so it should be 0.

accuracy_tables <- full_df %>% 
#  column_to_rownames("variable") %>% 
#  t() %>%
#  as.data.frame() %>% 
#  rownames_to_column("variable") %>% 
  group_by(Method) %>% 
  mutate(Disagree = sum(`False Negative`, `False Positive`, na.rm = T),
       Agree = sum(`True Negative` , `True Positive`, na.rm = T), 
       reconstructions_non_half = sum(Agree, Disagree, na.rm = T),
       reconstructions_all= sum(Agree, Disagree, Half, na.rm = T)) %>% 
  mutate(Accuracy = Agree / reconstructions_all, 
         Accuracy_incl_half = ((Agree + (Half/2)) / reconstructions_all), 
         Precision = `True Positive` / (`True Positive` + `False Positive` + Half), 
         Precision_incl_half = (`True Positive` + (Half*0.5)) / (`True Positive` + `False Positive`+ Half), 
         Recall = `True Positive` / (`True Positive` + `False Negative` + Half),
        Recall_incl_half = (`True Positive`+ (Half*0.5) )/ (`True Positive` + `False Negative` + Half)) %>%
    mutate(F1_score = 2 * ((Precision*Recall)/(Precision + Recall)), 
           F1_score_incl_half = 2 * ((Precision_incl_half *Recall_incl_half)/(Precision_incl_half + Recall_incl_half)) )%>% 
  ungroup %>% 
  mutate_if(is.numeric, round, 3) 

HL_findings_sheet <- read_csv(HL_findings_sheet_conflicts_fn) %>% 
  rename(Prediction = Value) %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, `Proto-language`)

accuracy_tables_to_write <- accuracy_tables %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("score") 

colnames(accuracy_tables_to_write) <- c("score", accuracy_tables_to_write[1,2:ncol(accuracy_tables_to_write)])
accuracy_tables_to_write <- accuracy_tables_to_write[-1, ]

accuracy_tables_to_write %>% 
  write_tsv("output/HL_comparison/accuracy_tables.tsv")


#OUTPUTTING XTABLES
OUTPUT_DIR <- file.path( OUTPUTDIR_plots , "results")
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)}

#outputting xtable of false pos, negative etc values
cap <- "Table showing the amount of False Negative, False Positive, Half, True Negative and True Positive results"
lbl <- "True_post_results_table"
align <- c("r", "p{4cm}","l", "l", "l","l", "l", "l") 

accuracy_tables %>% 
  dplyr::select(Method, "False Negative", "False Positive", "Half", "True Negative", "True Positive", "$$\\textbf{Total}$$" = "reconstructions_all" ) %>%
  rename("$$\\textbf{\\cellcolor{spec_color_red!50}{\\parbox{1.8cm}{\\raggedright False Positive}}}$$" = "False Positive" ) %>% 
  rename("$$\\textbf{\\cellcolor{spec_color_red!50}{\\parbox{1.8cm}{\\raggedright False Negative}}}$$" = "False Negative" ) %>% 
  rename("$$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{1.8cm}{\\raggedright True Positive}}}$$" = "True Positive" ) %>% 
  rename("$$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{1.8cm}{\\raggedright True Negative}}}$$" = "True Negative" ) %>% 
  rename("$$\\textbf{\\cellcolor{spec_color_yellow!50}{\\parbox{1.8cm}{\\raggedright Half}}}$$" = "Half" ) %>% 
#  rownames_to_column("Method") %>% 
  mutate("Method" = str_replace_all(Method, "_", " ")) %>% 
  mutate("Method" = str_replace_all(`Method`, "parsimony", "MP")) %>% 
  mutate("Method" = str_replace_all(`Method`, "gray mcct", "Gray et al (2009) - MCCT ")) %>% 
  mutate("Method" = str_replace_all(`Method`, "gray posteriors", "Gray et al (2009) - posteriors ")) %>% 
  mutate("Method" = str_replace_all(`Method`, "glottolog", "Glottolog")) %>% 
  mutate("Method" = str_replace_all(`Method`, "most common most common", "Most common")) %>% 
  rename("$$\\textbf{\\parbox{2cm}{\\raggedright Method}}$$" = "Method") %>% 
        xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path(OUTPUT_DIR , "table_false_pos_etc.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = FALSE,
                       booktabs = TRUE) 

df_for_bar_plot <- accuracy_tables %>%
  mutate(Method =str_replace_all(Method, "_", " ")) %>% 
  mutate(Method =str_replace_all(Method, "pars", "Pars")) %>% 
  mutate(Method =str_replace_all(Method, "gray", "Gray")) %>% 
  mutate(Method =str_replace_all(Method, "mcct", "(MCCT)")) %>% 
  mutate(Method =str_replace_all(Method, "posteriors", "(posteriors)")) %>% 
  mutate(Method =str_replace_all(Method, "most common most common", "Most Common"))

df_for_bar_plot$Method <- factor(df_for_bar_plot$Method, levels = c("Parsimony glottolog", "Parsimony Gray (MCCT)", "Parsimony Gray (posteriors)", "ML glottolog" , "ML Gray (MCCT)"  , "ML Gray (posteriors)", "Most Common" ))

p <- df_for_bar_plot %>% 
      ggplot() +
  geom_bar(aes(x = Method, y = Accuracy_incl_half, alpha = Accuracy_incl_half), fill = wes_palette("Zissou1", n = 4)[1], stat = "identity") +
  coord_cartesian(ylim=c(0.7, 1)) +
  theme_classic(base_size = 20) +
  scale_alpha(range = c(0.7, 1)) +
#  scale_fill_viridis(option = "D", begin = 0, end = 0.3) +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 50, hjust=1), 
        axis.title.x = element_blank()) +
  geom_text(aes(x = `Method`, y = Accuracy_incl_half +0.02, label = round(Accuracy_incl_half, 2)), size=8, colour = "black") +
  ylab("\"Accuracy\" including half")

ggsave(plot = p, file.path(OUTPUT_DIR ,"/barplot_accuracy_inl_half.png"), 
        height = 7, width = 7)


p <- df_for_bar_plot %>% 
  ggplot() +
  geom_bar(aes(x = Method, y = F1_score, alpha = F1_score), fill = wes_palette("Zissou1", n = 4)[3], stat = "identity") +
  coord_cartesian(ylim=c(0.7, 1)) +
  scale_alpha(range = c(0.5, 1)) +
  theme_classic(base_size = 20) +
  scale_fill_viridis() +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 50, hjust=1), 
        axis.title.x = element_blank()) +
  geom_text(aes(x = `Method`, y =F1_score+0.02, label = round(F1_score, 2)), size=8, colour = "black") +
  ylab("F1 score")

ggsave(plot = p, file.path(OUTPUT_DIR ,"/barplot_F1_score.png"), 
       height = 7, width = 7)


p <- df_for_bar_plot %>%
  dplyr::select(Method, Accuracy, Accuracy_incl_half, F1_score, F1_score_incl_half) %>% 
  reshape2::melt(id.vars = "Method") %>%
  mutate(variable = str_replace_all(variable, "_", " ")) %>% 
  mutate(variable = str_replace_all(variable, "Accuracy", "Concordance")) %>% 
  mutate(variable = str_replace_all(variable, "incl half", "(incl half) ")) %>% 
    ggplot() +
  geom_bar(aes(x = Method, y = value, fill = variable, alpha = value), stat = "identity") +
  coord_cartesian(ylim=c(0.7, 1)) +
  theme_classic(base_size = 20) +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 50, hjust=1), 
        axis.title = element_blank()) +
  geom_text(aes(x = `Method`, y =value+0.03, label = round(value, 2)), size=8, colour = "black") +
  scale_fill_manual(values= wes_palette("Zissou1", n = 4)) +
  theme(plot.margin = unit(c(0.2,0.2,0.2,1), "cm")) +
  facet_wrap(~variable, ncol = 1)

ggsave(plot = p, file.path(OUTPUT_DIR ,"/barplot_facet_scores.png"), width = 7, height = 12)



p <- df_for_bar_plot %>%
  dplyr::select(Method, Accuracy, Accuracy_incl_half) %>% 
  reshape2::melt(id.vars = "Method") %>%
  mutate(variable = str_replace_all(variable, "_", " ")) %>% 
  mutate(variable = str_replace_all(variable, "Accuracy", "Concordance")) %>% 
  mutate(variable = str_replace_all(variable, "incl half", "(incl half) ")) %>% 
  ggplot() +
  geom_bar(aes(x = Method, y = value, fill = variable, alpha = value), stat = "identity") +
  coord_cartesian(ylim=c(0.7, 1)) +
  theme_classic(base_size = 20) +
  scale_alpha(range = c(0.3, 1)) +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 50, hjust=1), 
        axis.title = element_blank()) +
  geom_text(aes(x = `Method`, y =value+0.03, label = round(value, 2)), size=8) +
  scale_fill_manual(values= wes_palette("Zissou1", n = 2)) +
  theme(plot.margin = unit(c(0.2,0.2,0.2,1), "cm")) +
  facet_wrap(~variable, ncol = 1)

ggsave(plot = p, file.path(OUTPUT_DIR ,"/barplot_facet_scores_exclude_f1.png"), width = 7, height = 10)

