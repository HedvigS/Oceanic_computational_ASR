source("01_requirements.R")
HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 
#function for getting node positions over ML objects
get_node_positions_SCM <- function(GB_asr_object_SCM_content){
  
  #GB_asr_object_SCM <- GB_ACR_all_SCM$content[[73]]
  
  GB_asr_object_SCM <- GB_asr_object_SCM_content[[1]]
  
  
  
  
  feature <- GB_asr_object_SCM$data %>% colnames() %>% .[2]
  tree <- GB_asr_object_SCM$phy
  
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
  
  df_lik_anc <- as.data.frame(GB_asr_object_SCM$states)
  df_lik_anc$Node <- seq(Ntip(tree) + 1, Ntip(tree) + nrow(df_lik_anc))   # i.e. count from ntips + 1 …to .. ntips + number of nodes
  df <- df_proto_nodes %>% 
    left_join(df_lik_anc, by = "Node") %>% 
    mutate(Feature_ID = feature) %>% 
    rename(`0`= "(1,R1)" , `1` = "(2,R1)")
  
  cat("I'm done with finding the ML proto-language states for feature ", feature, " for ", basename(dir), ".\n", sep = "")
  df
}



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
    mutate(min_percent_SCM_gray = min / (`nTips_state_0`+ `nTips_state_1`)) %>%
    dplyr::select(Feature_ID, ntips_SCM_gray = nTips, zeroes_SCM_gray = `nTips_state_0`, ones_SCM_gray = `nTips_state_1`, min_percent_SCM_gray)
  
  ###Gray ML
  GB_ACR_all_SCM <- readRDS(file.path(dir, "GB_SCM_gray_tree.rds")) %>% 
    left_join(value_count_df, by = "Feature_ID") %>% 
    filter(!is.na(ntips_SCM_gray))
  
  df_lik_anc_SCM_gray <- lapply(GB_ACR_all_SCM$content, get_node_positions_SCM) %>% bind_rows()
  
  
  df_lik_anc_SCM_gray$gray_SCM_prediction <- if_else(df_lik_anc_SCM_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_SCM_gray$`1` > 0.6, "Present", "Half")) 
  
  df_lik_anc_SCM_gray <- df_lik_anc_SCM_gray %>% 
    mutate(`0` = round(`0`)) %>% 
    mutate(`1` = round(`1`)) %>% 
    dplyr::select(Feature_ID, "Proto-language", gray_SCM_prediction,gray_SCM_prediction_0 = `0`, gray_SCM_prediction_1 = `1`)
  
  df <- HL_findings_sheet %>% 
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
  
  #Counting up trues per feature
  df$countTruePos <- rowSums(df == "True Positive", na.rm = T)
  
  df$countTrueNeg <- rowSums(df == "True Negative", na.rm = T)
  
  df$countTrue <- df$countTruePos + df$countTrueNeg
  
  
  df %>% 
    arrange(-countTrue) %>% 
    left_join(GB_df_desc, by = "Feature_ID") %>% 
    write_tsv(file.path(dir, "all_reconstructions.tsv") ) 
  
  df_non_erg <- df %>% 
    filter(!is.na(Prediction)) %>% 
    arrange(-countTrue) %>% 
    left_join(GB_df_desc, by = "Feature_ID") %>% 
    separate(Abbreviation, into = c("Feature_ID_abbreviation", "Abbreviation"), sep = " ") %>% 
    dplyr::select(Feature_ID, Abbreviation, Question, "Proto-language",  `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources", everything())
  
  
  df_non_erg %>% 
    write_tsv(file.path(dir, "HL_comparison.tsv"))
  
  cat("Done with ", dir, ".\n")
}