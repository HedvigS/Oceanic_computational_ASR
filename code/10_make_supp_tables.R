source("01_requirements.R")

#Make supplementary table of all GB features
GB_df_desc <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>% 
  filter(Binary_Multistate != "Multi") %>% 
  dplyr::select(`Feature\\_ID` = ID, Name)

cap <- "Table of Grambank fetures"
lbl <- "GB_features_table"
align <- c("r","p{3cm}", "p{12cm}") 

GB_df_desc %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "results", "GB_features_supp_table.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, tabular.environment = "longtable", floating = F) 


 ##make a table of all reconstructions that the 6 main methods agree on as present
all_reconstructions <- read_tsv("output/HL_comparison/all_reconstructions.tsv") %>% 
  distinct()

six_out_of_six_agree <- all_reconstructions %>% 
  reshape2::melt(id.vars = c("Feature_ID", "Proto-language")) %>% 
  filter(variable != "most_common_prediction") %>% 
  filter(!str_detect(value, "enough")) %>% 
  group_by(Feature_ID, `Proto-language`, value) %>% 
  summarise(n = n()) %>% 
  filter(n >= 6) %>% 
  filter(value == "Present") %>% 
  dplyr::select("Feature_ID", "Proto-language")

all_reconstructions_for_xtable <- all_reconstructions %>%
  inner_join(six_out_of_six_agree) %>% 
  rename(`Feature\\_ID` = Feature_ID,
    `Glottolog Parsimony`= glottolog_parsimony_prediction,
         `Gray parismony -MCCT` = gray_parsimony_prediction,
         `Gray parismony -posteriors` = gray_parsimony_prediction_posteriors,
         `Glottolog ML` =glottolog_ML_prediction,
          `Gray ML -MCCT` = gray_ML_prediction,
          `Gray ML -posteriors` = gray_ML_prediction_posteriors,
`Most Common`= most_common_prediction) 
  
cap <- "Table showing predictions where the six MP and ML methods agree on presence"
lbl <- "extra_predictions_table"
align <- c("p{1.5cm}", "p{1.5cm}","p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}", "p{2.5cm}") 

all_reconstructions_for_xtable %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "results/extra_predictions.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, floating = F, math.style.negative = F,
                       booktabs = TRUE, tabular.environment = "longtable") 

