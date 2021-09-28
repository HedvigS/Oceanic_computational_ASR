source("01_requirements.R")
source("fun_get_ASR_nodes.R")

#reading in old sheet with HL-predictions
HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

HL_findings_sheet_conflicts <- read_csv("data/HL_findings/HL_findings_conflicts.csv") %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts)

####

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with


value_count_df <- read_csv("output/gray_et_al_2009//ML/mcct/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_gray = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_ML_gray = nTips, zeroes_ML_gray =  nTips_state_0, ones_ML_gray =  nTips_state_1, min_percent_ML_gray)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

##Gray tree ML 
GB_ASR_RDS_ML_gray <- readRDS("output/gray_et_al_2009/ML/mcct/GB_ML_gray_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_ML_gray))

df_lik_anc_ML_gray <- lapply(GB_ASR_RDS_ML_gray$content, get_node_positions_ML) %>% bind_rows()

df_lik_anc_ML_gray$gray_ML_prediction <- if_else(df_lik_anc_ML_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_gray$`1` > 0.6, "Present", "Half")) 

automatic_predctions <- df_lik_anc_ML_gray %>% 
  mutate(`0` = round(`0` ,2)) %>% 
  mutate(`1` = round(`1` ,2)) %>% 
  dplyr::select(Feature_ID, "Proto-language", gray_ML_prediction,gray_ML_prediction_0 = `0`, gray_ML_prediction_1 = `1`)

##############################################################################

df <- HL_findings_sheets %>% 
  right_join(automatic_predctions, by = c("Feature_ID", "Proto-language")) 

df$`ML result (Gray et al 2009-tree)` <- if_else(df$gray_ML_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                 if_else(df$gray_ML_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                         if_else(df$gray_ML_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                 if_else(df$gray_ML_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                         
                                                                         ifelse(df$gray_ML_prediction == "Half", "Half", NA)))))

df <- value_count_df %>% 
  right_join(df, by = "Feature_ID") 

##Marking which results can't be included because they don't have enough languages

#ML Gray
df$`ML result (Gray et al 2009-tree)` <- if_else(df$ntips_ML_gray <  ntips_half_gray, "Not enough languages", df$`ML result (Gray et al 2009-tree)`)
df$gray_ML_prediction <- if_else(df$ntips_ML_gray <  ntips_half_gray, "Not enough languages", df$gray_ML_prediction)
df$gray_ML_prediction_1 <- ifelse(df$ntips_ML_gray <  ntips_half_gray, NA, df$gray_ML_prediction_1)
df$gray_ML_prediction_0 <- ifelse(df$ntips_ML_gray <  ntips_half_gray, NA, df$gray_ML_prediction_0)

#Counting up trues per feature
df$countTruePos <- rowSums(df == "True Positive", na.rm = T)
df$countTrueNeg <- rowSums(df == "True Negative", na.rm = T)
df$countTrue <- df$countTruePos + df$countTrueNeg

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc, by = "Feature_ID") %>% 
  write_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv")