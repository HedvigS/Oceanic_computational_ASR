source("01_requirements.R")

#Make supplementary table of all GB features
GB_df_desc <- read_tsv(GB_df_desc_fn) %>% 
  filter(Binary_Multistate != "Multi") %>% 
  dplyr::select(`Feature ID` = ID, Name)

cap <- "Table of Grambank fetures"
lbl <- "GB_features_table"
align <- c("r","p{3cm}", "p{12cm}") 

fn <- file.path( OUTPUTDIR_plots , "results", "GB_features_supp_table.tex")

GB_df_desc %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = fn, sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, tabular.environment = "longtable", floating = F) 

#something went awry with the quote marks, correcting here.
read_lines(file = fn) %>% str_replace_all(pattern = "\"\"\"\"\"\"\"\"", replacement = "\"") %>% 
  write_lines(file = fn)

 ##make a table of all reconstructions that the 6 main methods agree on as present
extra_reconstructions <- read_tsv("output/HL_comparison/extra_predictions.tsv") %>% 
  distinct() 

six_out_of_six_agree <- extra_reconstructions %>% 
  filter(!str_detect(method, "common")) %>% 
  filter(!str_detect(prediction, "enough")) %>% 
  group_by(Feature_ID, `Proto-language`, prediction) %>% 
  mutate(n = n(), .groups = "drop") %>% 
  ungroup() %>% 
  filter(n >= 6) %>% 
  filter(prediction == "Present") %>%  
  dplyr::select(Feature_ID, "Proto-language") %>% 
  distinct()

extra_reconstructions_for_xtable <- extra_reconstructions %>%
  inner_join(six_out_of_six_agree, by = c("Feature_ID", "Proto-language")) %>% 
  pivot_wider(id_cols = c("Feature_ID", "Proto-language"), names_from = "method", values_from = "prediction") %>% 
  dplyr::select(`Feature\\_ID` = Feature_ID,
    `Glottolog Parsimony`= parsimony_glottolog,
         `Gray parismony -MCCT` = parsimony_gray_mcct,
         `Gray parismony -posteriors` = parsimony_gray_posteriors,
         `Glottolog ML` =ML_glottolog,
          `Gray ML -MCCT` = ML_gray_mcct,
          `Gray ML -posteriors` = ML_gray_posteriors,
`Most Common`= most_common_most_common) 
  
cap <- "Table showing predictions where the six MP and ML methods agree on presence"
lbl <- "extra_predictions_table"
align <- c("p{1.5cm}", "p{1.5cm}","p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}")

extra_reconstructions_for_xtable %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "results/extra_predictions.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, floating = F, math.style.negative = F,
                       booktabs = TRUE, tabular.environment = "longtable") 

