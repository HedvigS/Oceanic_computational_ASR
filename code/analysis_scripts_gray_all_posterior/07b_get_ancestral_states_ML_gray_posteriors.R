source("01_requirements.R")
source("fun_get_ASR_nodes.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets
HL_findings_sheet <- read_tsv(HL_findings_sheet_fn)

HL_findings_sheet_conflicts <- read_csv(HL_findings_sheet_conflicts_fn, show_col_types = F) %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts, by = c("Proto-language", "Feature_ID", "Prediction"))

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv(glottolog_df_fn, show_col_types = F)  %>% 
  dplyr::select(Glottocode, classification, Name)

#parameter description
GB_df_desc <- read_tsv(GB_df_desc_fn, show_col_types = F) %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 
#Function for getting ancestral states for 4 specific nodes out of castor parsimony objects

dirs <- list.dirs("output/gray_et_al_2009/ML/results_by_tree/", recursive = F)

for(dir in dirs){
  #start
  
  #  dir <- dirs[1]  
  
  
  ##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with
  
  value_count_df <- read_csv(file.path(dir, "results.csv"), col_types = 
                               cols(
                                 Feature_ID = col_character(),
                                 LogLikelihood = col_double(),
                                 AICc = col_double(),
                                 pRoot0 = col_double(),
                                 pRoot1 = col_double(),
                                 q01 = col_double(),
                                 q10 = col_double(),
                                 nTips = col_double(),
                                 nTips_state_0 = col_double(),
                                 nTips_state_1 = col_double()                               )) %>% 
    mutate(min = pmin(`nTips_state_0`, `nTips_state_1`)) %>% 
    mutate(min_percent_ML_gray = min / (`nTips_state_0`+ `nTips_state_1`)) %>%
    dplyr::select(Feature_ID, ntips_ML_gray = nTips, zeroes_ML_gray = `nTips_state_0`, ones_ML_gray = `nTips_state_1`, min_percent_ML_gray, min_ML_gray = min)
  
  ###Gray ML
  GB_ACR_all_ML <- readRDS(file.path(dir, "GB_ML_gray_tree.rds")) %>% 
    left_join(value_count_df, by = "Feature_ID") %>% 
    filter(!is.na(ntips_ML_gray))
  
    df_lik_anc_ML_gray <- lapply(GB_ACR_all_ML$content, get_node_positions_ML) %>% bind_rows()
    

  df_lik_anc_ML_gray$gray_ML_prediction <- if_else(df_lik_anc_ML_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_gray$`1` > 0.6, "Present", "Half")) 
  
  df_lik_anc_ML_gray <- df_lik_anc_ML_gray %>% 
  #  mutate(`0` = round(`0`, digits = 2)) %>% 
  #  mutate(`1` = round(`1`, digits = 2)) %>% 
    dplyr::select(Feature_ID, "Proto-language", gray_ML_prediction,gray_ML_prediction_0 = `0`, gray_ML_prediction_1 = `1`)
  
  df <- HL_findings_sheets %>% 
    right_join(df_lik_anc_ML_gray, by = c("Feature_ID", "Proto-language")) 
  
    df$`ML result (Gray et al 2009-tree)` <- ifelse(df$gray_ML_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                          ifelse(df$gray_ML_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                                  ifelse(df$gray_ML_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                          ifelse(df$gray_ML_prediction == "Present" & df$Prediction == 0, "False Positive",           ifelse(df$gray_ML_prediction == "Half" & !is.na(df$Prediction), "Half", NA)))))
  
    df <- value_count_df %>% 
    right_join(df, by = "Feature_ID") 

  ##Marking which results can't be included because they don't have enough languages
  
  #ML gray
  df$gray_ML_prediction <- if_else(df$ntips_ML_gray <  ntips_half_gray, "Not enough languages", df$gray_ML_prediction)
  df$gray_ML_prediction_1 <- ifelse(df$ntips_ML_gray <  ntips_half_gray, NA, df$gray_ML_prediction_1)
  
  df$gray_ML_prediction_0 <- ifelse(df$ntips_ML_gray <  ntips_half_gray, NA, df$gray_ML_prediction_0)
  
  df %>% 
    left_join(GB_df_desc, by = "Feature_ID") %>%
    write_tsv(file.path(dir, "all_reconstructions.tsv") ) 
  
cat(paste0("Done with ", dir, ".\n") )
}