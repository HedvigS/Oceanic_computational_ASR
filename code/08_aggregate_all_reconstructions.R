source("01_requirements.R")

HL_findings_sheet <- HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv") %>%  
  filter(!is.na(Prediction)) %>% 
  distinct(Feature_ID, Prediction, `Proto-language`)

values_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  distinct(Feature_ID, ntips = ntips_parsimony_glottolog, zeroes_total= zeroes_parsimony_glottolog ,ones_total=  ones_parsimony_glottolog, min_percent = min_percent_parsimony_glottolog)

most_common_values_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  dplyr::select(most_common_prediction, 
                `Proto-language`, 
                Feature_ID, 
                ntip_most_common = ntip, 
                min_most_common = min, 
                min_percent_most_common,
                result_most_common) %>% 
  distinct() %>% 
  filter(most_common_prediction != "Not enough languages")

#reading in the results from each method and tree

#parsimony
parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, 
                `Proto-language`, 
                glottolog_parsimony_prediction,
                `Parsimony result (Glottolog-tree)`, 
                ntips_parsimony_glottolog, 
                min_percent_parsimony_glottolog, 
                min_parsimony_glottolog)  %>% 
  distinct() %>% 
  filter(glottolog_parsimony_prediction != "Not enough languages")

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>%
  dplyr::select(Feature_ID, 
                `Proto-language`, 
                gray_mcct_parsimony_prediction = gray_parsimony_prediction,
                `Parsimony result (Gray et al 2009-tree)` , 
                min_percent_parsimony_gray, 
                min_parsimony_gray,
                ntips_parsimony_gray, ) %>% 
  distinct() %>% 
  filter(gray_mcct_parsimony_prediction != "Not enough languages")

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  dplyr::select(Feature_ID, 
                `Proto-language`, 
                gray_posteriors_parsimony_prediction = gray_parsimony_prediction,
                ntips_parsimony_gray_posteriors = ntips_parsimony_gray,
                min_percent_parsimony_gray_posteriors = min_percent_parsimony_gray,
                min_parsimony_gray_posteriors = min_parsimony_gray,
                `Parsimony result (Gray et al 2009-tree posteriors)`
                )%>% 
  distinct() %>% 
  filter(gray_posteriors_parsimony_prediction != "Not enough languages")

#ML

ML_glottolog_df <- read_tsv("output/glottolog-tree//ML/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, 
                `Proto-language`, 
                glottolog_ML_prediction, 
                min_percent_ML_glottolog, 
                min_ML_glottolog, 
                ntips_ML_glottolog,
                `ML result (Glottolog-tree)`) %>% 
  #  full_join(most_common_values_df, by = c("Feature_ID", "Proto-language") ) %>% 
  #  mutate(glottolog_ML_prediction = ifelse(is.na(glottolog_ML_prediction),most_common_prediction , glottolog_ML_prediction)) %>% #bc the ML method fails when all the tips are the same state, such instances have an NA value in the ML results. We jsut replace those with the most common (the only) value to make it comparable to the parsimony results. Note that we're talking trees where every tip is the same, i.e. the state is the same for Proto-Polynesian, Proto-Oceanic etc. Not just the root!
  distinct() %>% 
  filter(glottolog_ML_prediction != "Not enough languages")

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, 
                `Proto-language`,
                gray_mcct_ML_prediction = gray_ML_prediction, 
                min_percent_ML_gray, 
                min_ML_gray, 
                `ML result (Gray et al 2009-tree)`, 
                ntips_ML_gray) %>% 
  #  full_join(most_common_values_df, by = c("Feature_ID", "Proto-language")   ) %>% 
  #  mutate(gray_mcct_ML_prediction = ifelse(is.na(gray_mcct_ML_prediction),most_common_prediction , gray_mcct_ML_prediction)) %>%  #bc the ML method fails when all the tips are the same state, such instances have an NA value in the ML results. We jsut replace those with the most common (the only) value to make it comparable to the parsimony results. Note that we're talking trees where every tip is the same, i.e. the state is the same for Proto-Polynesian, Proto-Oceanic etc. Not just the root!
  distinct() %>% 
  filter(gray_mcct_ML_prediction != "Not enough languages")

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  dplyr::select(Feature_ID, 
                `Proto-language`,
                `ML result (Gray et al 2009-tree posteriors)`,
                ntips_ML_gray_posteriors = ntips_ML_gray,
                min_ML_gray_posteriors = min_ML_gray, 
                min_percent_ML_gray_posteriors = min_percent_ML_gray,
                gray_posteriors_ML_prediction= gray_ML_prediction) %>% 
  #  full_join(most_common_values_df, by = c("Feature_ID", "Proto-language")) %>% 
  #  mutate(gray_posteriors_ML_prediction = ifelse(is.na(gray_posteriors_ML_prediction),most_common_prediction , gray_posteriors_ML_prediction)) %>%  #bc the ML method fails when all the tips are the same state, such instances have an NA value in the ML results. We just replace those with the most common (the only) value to make it comparable to the parsimony results. Note that we're talking trees where every tip is the same, i.e. the state is the same for Proto-Polynesian, Proto-Oceanic etc. Not just the root!
  distinct() %>% 
  filter(gray_posteriors_ML_prediction != "Not enough languages")

full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(parsimon_gray_posteriors_df, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(ML_glottolog_df, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(ML_gray_mcct, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(ML_gray_posteriors_df, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(most_common_values_df, by = c("Feature_ID", "Proto-language")) %>%
  full_join(HL_findings_sheet, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(values_df, by = "Feature_ID") %>% 
  dplyr::select(glottolog_parsimony_prediction, gray_mcct_parsimony_prediction, gray_posteriors_parsimony_prediction, 
                glottolog_ML_prediction, gray_mcct_ML_prediction, gray_posteriors_ML_prediction, 
                most_common_prediction, HL_prediction = Prediction, everything()) 

full_df %>% 
  write_tsv(file = "output/all_reconstructions_all_methods.tsv", na = "")
