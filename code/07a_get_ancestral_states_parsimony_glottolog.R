source("01_requirements.R")
source("fun_get_ASR_nodes.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets
HL_findings_sheet <- read_tsv("output/processed_data/HL_findings/HL_findings_for_comparison.tsv")

HL_findings_sheet_conflicts <- read_csv("data/HL_findings_conflicts.csv") %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts)

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with

value_count_df <- read_csv("output/glottolog-tree/parsimony/results.csv") %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_glottolog = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_glottolog = ntips, zeroes_parsimony_glottolog = `0`, ones_parsimony_glottolog = `1`, min_percent_parsimony_glottolog, min_parsimony_glottolog = min)
  
#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

###Glottolog parsimony
GB_ACR_all_parsimony <- readRDS("output/glottolog-tree/parsimony/GB_parsimony_Glottolog_tree_full.rds")

df_lik_anc_parsimony_glottolog <- lapply(GB_ACR_all_parsimony$content, get_node_positions_parsimony) %>% bind_rows()

df_lik_anc_parsimony_glottolog$glottolog_parsimony_prediction <- if_else(df_lik_anc_parsimony_glottolog$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_glottolog$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_glottolog <- df_lik_anc_parsimony_glottolog %>% 
#  mutate(`0` = round(`0`)) %>% 
#  mutate(`1` = round(`1`)) %>% 
  dplyr::select(Feature_ID, "Proto-language", glottolog_parsimony_prediction,glottolog_parsimony_prediction_0 = `0`, glottolog_parsimony_prediction_1 = `1`)

df <- HL_findings_sheets %>% 
  right_join(df_lik_anc_parsimony_glottolog, by = c("Feature_ID", "Proto-language")) 

##Marking more clearly results

df$`Parsimony result (Glottolog-tree)` <- if_else(df$glottolog_parsimony_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                        if_else(df$glottolog_parsimony_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                                if_else(df$glottolog_parsimony_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                        if_else(df$glottolog_parsimony_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                                
                                                                                ifelse(df$glottolog_parsimony_prediction == "Half" & !is.na(df$Prediction), "Half", NA)))))

df <- value_count_df %>% 
  right_join(df) 

##Marking which results can't be included because they don't have enough languages

#parsimony glottolog
df$`Parsimony result (Glottolog-tree)` <- if_else(df$ntips_parsimony_glottolog <  ntips_half_glottolog, "Not enough languages", df$`Parsimony result (Glottolog-tree)`)
df$glottolog_parsimony_prediction <- if_else(df$ntips_parsimony_glottolog <  ntips_half_glottolog, "Not enough languages", df$glottolog_parsimony_prediction)
df$glottolog_parsimony_prediction_1 <- ifelse(df$ntips_parsimony_glottolog <  ntips_half_glottolog, NA, df$glottolog_parsimony_prediction_1)
df$glottolog_parsimony_prediction_0 <- ifelse(df$ntips_parsimony_glottolog <  ntips_half_glottolog, NA, df$glottolog_parsimony_prediction_0)

#parameter description
GB_df_desc <- read_tsv(GB_df_desc_fn) %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df %>% 
  left_join(GB_df_desc, by = "Feature_ID") %>%
  write_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv")  