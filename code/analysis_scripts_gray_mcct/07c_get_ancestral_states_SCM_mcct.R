source("01_requirements.R")
#reading in old sheet with HL-predictions

HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

HL_findings_sheet_conflicts <- read_csv("data/HL_findings/HL_findings_conflicts.csv") %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <- HL_findings_sheet %>% 
  full_join(HL_findings_sheet_conflicts)

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with

value_count_df <- read_csv("output/gray_et_al_2009/SCM/mcct/results.csv") %>% 
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_SCM_gray = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_SCM_gray = nTips, zeroes_SCM_gray =  nTips_state_0, ones_SCM_gray =  nTips_state_1, min_percent_SCM_gray)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

##Gray SCM rdata object
GB_ASR_RDS_SCM_gray <- readRDS("output/gray_et_al_2009/SCM/mcct/GB_SCM_gray_tree_100.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_SCM_gray))

########################################## SCM ##########################################
get_node_positions_SCM <- function(GB_asr_object_SCM){
  
  #GB_asr_object_SCM <- GB_ASR_RDS_SCM_gray$content[[1]]

  GB_asr_object_scm_SIMMAP_summary <- GB_asr_object_SCM[[1]]
  GB_asr_object_scm_results_df <- GB_asr_object_SCM[[2]]
  
  feature <- GB_asr_object_scm_results_df$Feature_ID
  tree <- GB_asr_object_SCM[[4]]
  
  tip_label_df <- tree$tip.label %>% 
    as.data.frame() %>% 
    rename("Glottocode" = ".") 
  
  Polynesian_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "poly1242")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  polynesian_node <- getMRCA(tree, Polynesian_tips)
  
  oceanic_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "ocea1241")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  oceanic_node <- getMRCA(tree, oceanic_tips)
  
  central_oceanic_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "cent2060")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  central_oceanic_node <- getMRCA(tree, central_oceanic_tips)
  
  eastern_polynesian_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "east2449")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  eastern_poly_node <- getMRCA(tree, eastern_polynesian_tips)
  
  df_proto_nodes <- tibble("Proto-language" = c("Proto-Oceanic", "Proto-Central Pacific", "Proto-Polynesian", "Proto-Eastern Polynesian") , Node = c(oceanic_node,  central_oceanic_node,  polynesian_node, eastern_poly_node))
  
    df_lik_anc <-     GB_asr_object_SCM[[1]]$ace %>% 
      as.data.frame() %>% 
      rownames_to_column("Node") 

      df <- df_proto_nodes %>% 
        mutate(Node = as.character(Node)) %>% 
        left_join(df_lik_anc, by = "Node") %>% 
    mutate(Feature_ID = feature)
      
  cat("I'm done with finding the SCM proto-language states for feature ", feature, ".\n", sep = "")
  df
}


df_lik_anc_SCM_gray <- lapply(GB_ASR_RDS_SCM_gray$content, get_node_positions_SCM) %>% bind_rows()

cat("I'm done with finding the SCM proto-language states for all featres.\n", sep = "")

df_lik_anc_SCM_gray$gray_mcct_scm_prediction <- if_else(df_lik_anc_SCM_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_SCM_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_SCM_gray  <- df_lik_anc_SCM_gray %>% 
  mutate(`0` = round(`0`)) %>% 
  mutate(`1` = round(`1`)) %>% 
  dplyr::select(Feature_ID, "Proto-language", gray_mcct_scm_prediction,gray_mcct_scm_prediction_0 = `0`, gray_mcct_scm_prediction_1 = `1`)

df <- HL_findings_sheets %>% 
  right_join(df_lik_anc_SCM_gray, by = c("Feature_ID", "Proto-language")) 

##Marking more clearly results
df$`SCM result (Gray et al 2009-tree MCCT)` <- if_else(df$gray_mcct_scm_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                 if_else(df$gray_mcct_scm_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                         if_else(df$gray_mcct_scm_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                 if_else(df$gray_mcct_scm_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                         
                                                                         ifelse(df$gray_mcct_scm_prediction == "Half", "Half", NA)))))


df <- value_count_df %>% 
  right_join(df) 

##Marking which results can't be included because they don't have enough languages

#ML Gray
df$`SCM result (Gray et al 2009-tree MCCT)` <- if_else(df$ntips_SCM_gray <  ntips_half_gray, "Not enough languages", df$`SCM result (Gray et al 2009-tree MCCT)`)
df$gray_mcct_scm_prediction <- if_else(df$ntips_SCM_gray <  ntips_half_gray, "Not enough languages", df$gray_mcct_scm_prediction)
df$gray_mcct_scm_prediction_1 <- ifelse(df$ntips_SCM_gray <  ntips_half_gray, NA, df$gray_mcct_scm_prediction_1)
df$gray_mcct_scm_prediction_0 <- ifelse(df$ntips_SCM_gray <  ntips_half_gray, NA, df$gray_mcct_scm_prediction_0)

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df %>% 
  left_join(GB_df_desc) %>% 
  write_tsv("output/gray_et_al_2009/SCM/mcct/all_reconstructions.tsv")  
