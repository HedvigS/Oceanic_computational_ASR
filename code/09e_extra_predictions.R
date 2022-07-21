source("01_requirements.R")

GB_ID_desc <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Name)

#OUTPUTTING XTABLES
OUTPUT_DIR <- file.path( OUTPUTDIR_plots , "results")
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)}

#HL sheets
HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  distinct(Feature_ID, `Proto-language`)

HL_findings_sheet_conflicts <- read_csv(HL_findings_sheet_conflicts_fn) %>% 
  rename(Prediction = Value) %>% 
  filter(!is.na(Prediction)) %>% 
  distinct(Feature_ID, `Proto-language`)

HL_findings_sheets <- full_join(HL_findings_sheet, HL_findings_sheet_conflicts, by = c("Proto-language", "Feature_ID"))

#the results per method
most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  dplyr::select("Feature_ID", "Proto-language", "most_common_prediction")

parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  dplyr::select("Feature_ID", "Proto-language", "glottolog_parsimony_prediction")

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  dplyr::select("Feature_ID", "Proto-language", "gray_parsimony_prediction")

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  dplyr::select("Feature_ID", "Proto-language", "gray_parsimony_prediction_posteriors" = gray_parsimony_prediction)
  
ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  dplyr::select("Feature_ID", "Proto-language", "glottolog_ML_prediction")

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  dplyr::select("Feature_ID", "Proto-language", "gray_ML_prediction")

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  dplyr::select("Feature_ID", "Proto-language", gray_ML_prediction_posteriors = gray_ML_prediction)

#combining all
full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_df)

full_df %>% 
write_tsv("output/HL_comparison/all_reconstructions.tsv", na = "")

full_df <- full_df %>% 
anti_join(HL_findings_sheets, by = c("Feature_ID", "Proto-language") ) 
  

full_df %>% 
  reshape2::melt(id.vars = c("Feature_ID", "Proto-language")) %>% 
  group_by(Feature_ID, `Proto-language`, value) %>% 
  summarise(n = n()) %>% 
  left_join(GB_ID_desc) %>% 
  write_tsv("output/HL_comparison/extra_predictions.tsv")