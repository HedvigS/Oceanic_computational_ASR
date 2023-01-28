source("01_requirements.R")
source("fun_get_ASR_nodes.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets
HL_findings_sheet <- read_tsv(HL_findings_sheet_fn)

HL_findings_sheet_conflicts <- read_csv(HL_findings_sheet_conflicts_fn) %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts)

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with

value_count_df<- read_csv("output/glottolog-tree/ML/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_glottolog = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_ML_glottolog = nTips, zeroes_ML_glottolog =  nTips_state_0, ones_ML_glottolog =  nTips_state_1, min_percent_ML_glottolog, min_ML_glottolog = min)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv(glottolog_df_fn, show_col_types = F)  %>% 
  dplyr::select(Glottocode, classification, Name)

##Glottolog ML
#rds object with all the output of ML on the glottolog tree

GB_ASR_RDS_ML_glottolog <- readRDS("output/glottolog-tree/ML/GB_ML_glottolog_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_ML_glottolog))

df_lik_anc_ML_glottolog <- lapply(GB_ASR_RDS_ML_glottolog$content, get_node_positions_ML) %>% bind_rows()

df_lik_anc_ML_glottolog$glottolog_ML_prediction <- if_else(df_lik_anc_ML_glottolog$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_glottolog$`1` > 0.6, "Present", "Half")) 

automatic_predctions<- df_lik_anc_ML_glottolog %>% 
  dplyr::select(Feature_ID, "Proto-language", glottolog_ML_prediction,glottolog_ML_prediction_0 = `0`, glottolog_ML_prediction_1 = `1` ) #%>%
#  mutate(glottolog_ML_prediction_0 = round(glottolog_ML_prediction_0 ,2)) %>% 
#  mutate(glottolog_ML_prediction_0 = round(glottolog_ML_prediction_0 ,2)) 

df <- HL_findings_sheets %>% 
  right_join(automatic_predctions, by = c("Feature_ID", "Proto-language")) 

##Marking more clearly results
df$`ML result (Glottolog-tree)` <- if_else(df$glottolog_ML_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                           if_else(df$glottolog_ML_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                   if_else(df$glottolog_ML_prediction == "Absent" & 
                                                             df$Prediction == 1, "False Negative",  
                                                           if_else(df$glottolog_ML_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                   
                                                                   ifelse(df$glottolog_ML_prediction == "Half", "Half", NA)))))

df <- value_count_df %>% 
  right_join(df) 

##Marking which results can't be included because they don't have enough languages

#ML Glottolog
df$`ML result (Glottolog-tree)` <- if_else(df$ntips_ML_glottolog <  ntips_half_glottolog, "Not enough languages", df$`ML result (Glottolog-tree)` )
df$glottolog_ML_prediction <- if_else(df$ntips_ML_glottolog <  ntips_half_glottolog, "Not enough languages", df$glottolog_ML_prediction)
df$glottolog_ML_prediction_1 <- ifelse(df$ntips_ML_glottolog <  ntips_half_glottolog, NA, df$glottolog_ML_prediction_1 )
df$glottolog_ML_prediction_0 <- ifelse(df$ntips_ML_glottolog <  ntips_half_glottolog, NA, df$glottolog_ML_prediction_0 )

#parameter description
GB_df_desc <- read_tsv(GB_df_desc_fn) %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df %>% 
  left_join(GB_df_desc, by = "Feature_ID") %>% 
  write_tsv("output/glottolog-tree/ML/all_reconstructions.tsv")  
