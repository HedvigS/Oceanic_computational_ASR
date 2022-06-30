source("01_requirements.R")
source("fun_get_ASR_nodes.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets

HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

HL_findings_sheet_conflicts <- read_csv("data/HL_findings/HL_findings_conflicts.csv") %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts)

value_count_df <-read_csv("output/glottolog_tree_binary/SCM/results.csv") %>% 
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_SCM_glottolog = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_SCM_glottolog = nTips, zeroes_SCM_glottolog =  nTips_state_0, ones_SCM_glottolog =  nTips_state_1, min_percent_SCM_glottolog)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

##glottolog scm object
GB_ASR_RDS_SCM_glottolog <- readRDS("output/glottolog_tree_binary/SCM/GB_SCM_glottolog_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_SCM_glottolog))


df_lik_anc_SCM_glottolog <- lapply(GB_ASR_RDS_SCM_glottolog$content, get_node_positions_SCM) %>% bind_rows()

df_lik_anc_SCM_glottolog$glottolog_SCM_prediction <- if_else(df_lik_anc_SCM_glottolog$`0` > 0.6, "Absent", if_else(df_lik_anc_SCM_glottolog$`1` > 0.6, "Present", "Half")) 

automatic_predctions  <- df_lik_anc_SCM_glottolog %>% 
#  mutate(`0` = round(`0` ,2)) %>% 
#  mutate(`1` = round(`1` ,2)) %>% 
  dplyr::select(Feature_ID, "Proto-language", glottolog_SCM_prediction,glottolog_SCM_prediction_0 = `0`, glottolog_SCM_prediction_1 = `1`)

df <- HL_findings_sheet %>% 
  right_join(automatic_predctions, by = c("Feature_ID", "Proto-language")) 

##Marking more clearly results

df$`SCM result (glottolog et al 2009-tree)` <- if_else(df$glottolog_SCM_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                 if_else(df$glottolog_SCM_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                         if_else(df$glottolog_SCM_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                 if_else(df$glottolog_SCM_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                         
                                                                         ifelse(df$glottolog_SCM_prediction == "Half", "Half", NA)))))

df <- value_count_df %>% 
  right_join(df) 

##Marking which results can't be included because they don't have enough languages

#ML glottolog
df$`SCM result (glottolog et al 2009-tree)` <- if_else(df$ntips_SCM_glottolog <  ntips_half_glottolog, "Not enough languages", df$`SCM result (glottolog et al 2009-tree)`)
df$glottolog_SCM_prediction <- if_else(df$ntips_SCM_glottolog <  ntips_half_glottolog, "Not enough languages", df$glottolog_SCM_prediction)
df$glottolog_SCM_prediction_1 <- ifelse(df$ntips_SCM_glottolog <  ntips_half_glottolog, NA, df$glottolog_SCM_prediction_1)
df$glottolog_SCM_prediction_0 <- ifelse(df$ntips_SCM_glottolog <  ntips_half_glottolog, NA, df$glottolog_SCM_prediction_0)

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df %>% 
  left_join(GB_df_desc) %>% 
  write_tsv("output/glottolog_tree_binary/SCM/all_reconstructions.tsv")  