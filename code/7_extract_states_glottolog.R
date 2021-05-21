source("1_requirements.R")

#reading in old sheet with HL-predictions
HL_findings_sheet <- read_csv(file.path("data", "HL_findings", "HL_findings.csv")) %>% 
  dplyr::select(`Grambank ID`, "Proto-language", Prediction = `Finding from Historical Linguistics`, "Historical Linguistics sources") 

#reading in old sheet with HL-predictions - ergative section
HL_findings_sheet_erg <- read_csv(file.path("data", "HL_findings", "HL_findings_erg.csv")) %>% 
  dplyr::select(`Grambank ID`, "Proto-language", Prediction = `Finding from Historical Linguistics`, "Historical Linguistics sources") 

HL_findings_sheet <- HL_findings_sheet_erg %>% 
  full_join(HL_findings_sheet)

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with

value_counts_parsimony_glottolog <- read_csv("output/glottolog_tree_binary/parsimony/results.csv") %>%
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_glottolog = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_glottolog = ntips, zeroes_parsimony_glottolog = `0`, ones_parsimony_glottolog = `1`, min_percent_parsimony_glottolog) 

value_counts_ML_glottolog <- read_csv("output/glottolog_tree_binary/ML/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_glottolog = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_ML_glottolog = nTips, zeroes_ML_glottolog =  nTips_state_0, ones_ML_glottolog =  nTips_state_1, min_percent_ML_glottolog)

value_count_df <- value_counts_ML_glottolog %>% 
  full_join(value_counts_parsimony_glottolog)

rm(value_counts_ML_glottolog, value_counts_parsimony_glottolog)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

#Function for getting ancestral states for 4 specific nodes out of the corHMM product (ML).
get_node_positions <- function(GB_asr_object_ml){
  
  #GB_asr_object_ml <- GB_ASR_RDS_ML_gray$content[[1]][[1]]
  
  GB_asr_object_ml <- GB_asr_object_ml[[1]]
  
  feature <- GB_asr_object_ml$data %>% colnames() %>% .[2]
  tree <- GB_asr_object_ml$phy
  
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
  
  df_lik_anc <- as.data.frame(GB_asr_object_ml$states)
  df_lik_anc$Node <- seq(Ntip(tree) + 1, Ntip(tree) + nrow(df_lik_anc))   # i.e. count from ntips + 1 …to .. ntips + number of nodes
  df <- df_proto_nodes %>% 
    inner_join(df_lik_anc, by = "Node") %>% 
    mutate(`Grambank ID` = feature) %>% 
    rename(`0`= "(1,R1)" , `1` = "(2,R1)")
  
  cat("I'm done with finding the ML proto-language states for feature ", feature, ".\n", sep = "")
  df
}


##Glottolog ML
#rds object with all the output of ML on the glottolog tree

GB_ASR_RDS_ML_glottolog <- readRDS("output/glottolog_tree_binary/ML/GB_ML_glottolog_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_ML_glottolog))

df_lik_anc_ML_glottolog <- lapply(GB_ASR_RDS_ML_glottolog$content, get_node_positions) %>% bind_rows()

df_lik_anc_ML_glottolog$glottolog_ML_prediction <- if_else(df_lik_anc_ML_glottolog$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_glottolog$`1` > 0.6, "Present", "Half")) 

df_lik_anc_ML_glottolog <- df_lik_anc_ML_glottolog %>% 
  mutate(`0` = round(`0` ,2)) %>% 
  mutate(`1` = round(`1` ,2)) %>% 
  dplyr::select(`Grambank ID`, "Proto-language", glottolog_ML_prediction,glottolog_ML_prediction_0 = `0`, glottolog_ML_prediction_1 = `1`)


#Function for getting ancestral states for 4 specific nodes out of castor parsimony objects
get_node_positions_parsimony <- function(GB_asr_object_parsimony){
  
  #  GB_asr_object_parsimony <- GB_ACR_all_parsimony$content[[187]]
  
  feature <- GB_asr_object_parsimony[[1]]
  ASR_object <- GB_asr_object_parsimony[[2]]
  tree <- GB_asr_object_parsimony[[4]]
  
  tip_label_df <- tree$tip.label %>% 
    as.data.frame() %>% 
    rename("Name" = ".") %>% 
    left_join(glottolog_df, by = "Name") 
  
  Polynesian_tips <- tip_label_df %>% 
    filter(str_detect(classification, "poly1242")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  polynesian_node <- getMRCA(tree, Polynesian_tips)
  
  oceanic_tips <- tip_label_df %>% 
    filter(str_detect(classification, "ocea1241")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  oceanic_node <- getMRCA(tree, oceanic_tips)
  
  central_oceanic_tips <- tip_label_df %>% 
    filter(str_detect(classification, "cent2060")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  central_oceanic_node <- getMRCA(tree, central_oceanic_tips)
  
  eastern_polynesian_tips <- tip_label_df %>% 
    filter(str_detect(classification, "east2449")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  eastern_poly_node <- getMRCA(tree, eastern_polynesian_tips)
  
  df_proto_nodes <- tibble("Proto-language" = c("Proto-Oceanic", "Proto-Central Pacific", "Proto-Polynesian", "Proto-Eastern Polynesian") , Node = c(oceanic_node,  central_oceanic_node,  polynesian_node, eastern_poly_node))
  
  #  df_proto_nodes$Node <-   df_proto_nodes$Node - 1
  
  df_lik_anc <- as.data.frame(ASR_object$ancestral_likelihoods)
  df_lik_anc$Node <- seq(Ntip(tree) + 1, Ntip(tree) + nrow(df_lik_anc))  
  colnames(df_lik_anc) <- c("0", "1", "Node") 
  
  df <- df_proto_nodes %>% 
    left_join(df_lik_anc, by = "Node") %>% 
    mutate(`Grambank ID` = feature)
  
  cat("I'm done with finding the parsimony proto-language states for feature ", feature, ".\n", sep = "")
  
  df
}



###Glottolog parsimony
GB_ACR_all_parsimony <- readRDS("output/glottolog_tree_binary/parsimony/GB_parsimony_Glottolog_tree_full.rds")

df_lik_anc_parsimony_glottolog <- lapply(GB_ACR_all_parsimony$content, get_node_positions_parsimony) %>% bind_rows()

df_lik_anc_parsimony_glottolog$glottolog_parsimony_prediction <- if_else(df_lik_anc_parsimony_glottolog$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_glottolog$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_glottolog <- df_lik_anc_parsimony_glottolog %>% 
  mutate(`0` = round(`0`)) %>% 
  mutate(`1` = round(`1`)) %>% 
  dplyr::select(`Grambank ID`, "Proto-language", glottolog_parsimony_prediction,glottolog_parsimony_prediction_0 = `0`, glottolog_parsimony_prediction_1 = `1`)




cat("Done with retreiving the particular states for 4 proto-languages for all features for the glottolog-tree.")

##comparing gray and glottolog

computational_predictions_all_df <- df_lik_anc_ML_glottolog  %>% 
  full_join(df_lik_anc_parsimony_glottolog) 

computational_predictions_HL_compare_df <- HL_findings_sheet %>% 
  left_join(computational_predictions_all_df)

computational_predictions_HL_compare_df$`ML result (Glottolog-tree)` <- if_else(computational_predictions_HL_compare_df$glottolog_ML_prediction == "Present" & computational_predictions_HL_compare_df$Prediction == "Present", "True Positive",  
                                           if_else(computational_predictions_HL_compare_df$glottolog_ML_prediction == "Absent" & computational_predictions_HL_compare_df$Prediction == "Absent", "True Negative",   
                                                   if_else(computational_predictions_HL_compare_df$glottolog_ML_prediction == "Absent" & computational_predictions_HL_compare_df$Prediction == "Present", "False Negative",  
                                                           if_else(computational_predictions_HL_compare_df$glottolog_ML_prediction == "Present" & computational_predictions_HL_compare_df$Prediction == "Absent", "False Positive",
                                                                   
                                                                   ifelse(computational_predictions_HL_compare_df$glottolog_ML_prediction == "Half", "Half", NA)))))

computational_predictions_HL_compare_df$`Parsimony result (Glottolog-tree)` <- if_else(computational_predictions_HL_compare_df$glottolog_parsimony_prediction == "Present" & computational_predictions_HL_compare_df$Prediction == "Present", "True Positive",  
                                                  if_else(computational_predictions_HL_compare_df$glottolog_parsimony_prediction == "Absent" & computational_predictions_HL_compare_df$Prediction == "Absent", "True Negative",   
                                                          if_else(computational_predictions_HL_compare_df$glottolog_parsimony_prediction == "Absent" & computational_predictions_HL_compare_df$Prediction == "Present", "False Negative",  
                                                                  if_else(computational_predictions_HL_compare_df$glottolog_parsimony_prediction == "Present" & computational_predictions_HL_compare_df$Prediction == "Absent", "False Positive",
                                                                          
                                                                          ifelse(computational_predictions_HL_compare_df$glottolog_parsimony_prediction == "Half", "Half", NA)))))

computational_predictions_HL_compare_df <- value_count_df %>% 
  rename(`Grambank ID` = Feature_ID) %>% 
  right_join(computational_predictions_HL_compare_df)

computational_predictions_HL_compare_df$`ML result (Glottolog-tree)` <- if_else(computational_predictions_HL_compare_df$ntips_ML_glottolog <  117, "Not enough languages", computational_predictions_HL_compare_df$`ML result (Glottolog-tree)` )
computational_predictions_HL_compare_df$`Parsimony result (Glottolog-tree)` <- if_else(computational_predictions_HL_compare_df$ntips_parsimony_glottolog <  117, "Not enough languages", computational_predictions_HL_compare_df$`Parsimony result (Glottolog-tree)`)

#Counting up trues per feature
computational_predictions_HL_compare_df$countTruePos <- rowSums(computational_predictions_HL_compare_df == "True Positive", na.rm = T)

computational_predictions_HL_compare_df$countTrueNeg <- rowSums(computational_predictions_HL_compare_df == "True Negative", na.rm = T)

computational_predictions_HL_compare_df$countTrue <- computational_predictions_HL_compare_df$countTruePos + computational_predictions_HL_compare_df$countTrueNeg

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(`Grambank ID` = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df_pruned_erg <- computational_predictions_HL_compare_df %>% 
  arrange(`Proto-language`, `Grambank ID`) %>% 
  filter(`Grambank ID` == "GB409" |
           `Grambank ID` == "GB408" |
           `Grambank ID` == "GB410" |
           `Grambank ID` == "GB147" )   %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Grambank ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select(`Grambank ID` = `Grambank ID`, Abbreviation, Question, "Proto-language", `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)`)

df_pruned_erg %>% 
  write_tsv("output/HL_comparison/HL_comparison_erg_glottolog_tree.tsv")

df_non_erg <- computational_predictions_HL_compare_df %>% 
  anti_join(df_pruned_erg) %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Grambank ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select(`Grambank ID` = `Grambank ID`, Abbreviation, Question, "Proto-language",  `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` )


df_non_erg %>% 
  write_tsv("output/HL_comparison/HL_comparison_glottolog_tree.tsv")

computational_predictions_all_df %>%
  write_tsv("output/HL_comparison/glottolog_tree_predictions_all.tsv")


