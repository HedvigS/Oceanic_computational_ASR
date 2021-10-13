source("01_requirements.R")
source("fun_get_ASR_nodes.R")

HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

HL_findings_sheet_conflicts <- read_csv("data/HL_findings/HL_findings_conflicts.csv") %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 
#function for getting node positions over ML objects

dirs <- list.dirs("output/gray_et_al_2009/SCM/results_by_tree", recursive = F)

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
    mutate(min_percent_SCM_gray = min / (`nTips_state_0`+ `nTips_state_1`)) %>%
    dplyr::select(Feature_ID, ntips_SCM_gray = nTips, zeroes_SCM_gray = `nTips_state_0`, ones_SCM_gray = `nTips_state_1`, min_percent_SCM_gray)
  
  ###Gray ML
  GB_ACR_all_SCM <- readRDS(file.path(dir, "GB_SCM_gray_tree.rds")) %>% 
    left_join(value_count_df, by = "Feature_ID") %>% 
    filter(!is.na(ntips_SCM_gray))
  
  df_lik_anc_SCM_gray <- lapply(GB_ACR_all_SCM$content, get_node_positions_SCM) %>% bind_rows()
  
  
  df_lik_anc_SCM_gray$gray_SCM_prediction <- if_else(df_lik_anc_SCM_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_SCM_gray$`1` > 0.6, "Present", "Half")) 
  
  df_lik_anc_SCM_gray <- df_lik_anc_SCM_gray %>% 
#    mutate(`0` = round(`0`, digits = 2)) %>% 
#    mutate(`1` = round(`1`, digits = 2)) %>% 
    dplyr::select(Feature_ID, "Proto-language", gray_SCM_prediction,gray_SCM_prediction_0 = `0`, gray_SCM_prediction_1 = `1`)
  
  df <- HL_findings_sheets %>% 
    right_join(df_lik_anc_SCM_gray, by = c("Feature_ID", "Proto-language")) 
  
  df$`ML result (Gray et al 2009-tree)` <- ifelse(df$gray_SCM_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                  ifelse(df$gray_SCM_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                         ifelse(df$gray_SCM_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                ifelse(df$gray_SCM_prediction == "Present" & df$Prediction == 0, "False Positive",           ifelse(df$gray_SCM_prediction == "Half", "Half", NA)))))
  
  df <- value_count_df %>% 
    right_join(df, by = "Feature_ID") 
  
  ##Marking which results can't be included because they don't have enough languages
  
  #ML gray
  df$gray_SCM_prediction <- if_else(df$ntips_SCM_gray <  ntips_half_gray, "Not enough languages", df$gray_SCM_prediction)
  df$gray_SCM_prediction_1 <- ifelse(df$ntips_SCM_gray <  ntips_half_gray, NA, df$gray_SCM_prediction_1)
  
  df$gray_SCM_prediction_0 <- ifelse(df$ntips_SCM_gray <  ntips_half_gray, NA, df$gray_SCM_prediction_0)
  df %>% 
    left_join(GB_df_desc, by = "Feature_ID") %>% 
    write_tsv(file.path(dir, "all_reconstructions.tsv") ) 

  cat("Done with ", dir, ".\n")
}