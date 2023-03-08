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

df_agreement_without_common %>% 
  anti_join(df_agreement) %>% 
left_join(GB_ID_desc, by = "Feature_ID") %>% View()



  filter(n == 7) %>% 
  left_join(GB_ID_desc, by = "Feature_ID") %>% View()
  write_tsv("output/HL_comparison/extra_predictions_all_agree.tsv")


  x <- read_tsv(file = "output/all_reconstructions_all_methods_wide_easy.tsv", na = "")