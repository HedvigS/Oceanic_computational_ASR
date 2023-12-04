source("01_requirements.R")

GB_ID_desc <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Name)

#OUTPUTTING XTABLES
OUTPUT_DIR <- file.path( OUTPUTDIR_plots , "results")
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)}

#HL sheets
full_df <- read_tsv("output/all_reconstructions_all_methods_long.tsv", col_types = cols(.default = "c")) %>% 
  filter(variable == "result"|variable == "prediction") %>% 
  filter(is.na(conflict)) %>% 
  dplyr::select(-conflict) %>% 
  unite(method, tree_type, sep = "_", col = "method") %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>% 
  filter(is.na(result)) %>% 
  filter(!is.na(`Proto-language`)) %>% 
  dplyr::select(-result)

full_df  %>% 
  write_tsv("output/HL_comparison/extra_predictions.tsv", na = "")

df_agreement <- full_df %>% 
  filter(!str_detect(method, "HL")) %>% 
  group_by(Feature_ID, `Proto-language`, prediction) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  filter(prediction == "Present") %>% 
  filter(n == 7) %>% 
  dplyr::select(-n)

df_agreement_without_common <- full_df %>% 
  filter(!str_detect(method, "common")) %>% 
  filter(!str_detect(method, "HL")) %>% 
  group_by(Feature_ID, `Proto-language`, prediction) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  filter(prediction == "Present") %>% 
  filter(n == 6) %>% 
  dplyr::select(-n)

df_agreement_without_common <- df_agreement_without_common %>% 
  anti_join(df_agreement, by = c("Feature_ID", "Proto-language", "prediction")) %>% 
  left_join(GB_ID_desc, by = "Feature_ID") %>% 
  rename(Feature = Feature_ID) %>% 
  dplyr::select(-prediction)

cap <- "Table showing the four Grambank features that were predicted as present by ML and MP in all three trees, but were not the most common feature in all languages"
lbl <- "table_extra_predictions_four"
align <- c("r", "p{2cm}","p{4cm}","p{8cm}" ) 

colnames(df_agreement_without_common) <- paste0("\\textbf{", colnames(df_agreement_without_common), "}")

df_agreement_without_common %>% 
  xtable(caption = cap, label = lbl,
         align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUT_DIR , "extra_predictions_minus_MC.tex"),
                       sanitize.colnames.function = function(x){x},
                       math.style.negative = F,
                       booktabs = TRUE,
#                       hline.after = c(-1, 0, nrow(df_agreement_without_common)),
                       include.rownames = FALSE)
  
