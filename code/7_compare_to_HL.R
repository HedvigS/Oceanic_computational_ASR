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

value_counts_parsimony_gray <- read_csv("output/gray_et_al_2009/parsimony/results.csv") %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_gray = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_gray = ntips, zeroes_parsimony_gray = `0`, ones_parsimony_gray = `1`, min_percent_parsimony_gray)

value_counts_ML_glottolog <- read_csv("output/glottolog_tree_binary/ML/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_glottolog = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_ML_glottolog = nTips, zeroes_ML_glottolog =  nTips_state_0, ones_ML_glottolog =  nTips_state_1, min_percent_ML_glottolog)

value_counts_ML_gray <- read_csv("output/gray_et_al_2009//ML/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_gray = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_ML_gray = nTips, zeroes_ML_gray =  nTips_state_0, ones_ML_gray =  nTips_state_1, min_percent_ML_gray)
  
value_count_df <- value_counts_ML_gray %>% 
  full_join(value_counts_ML_glottolog) %>%
  full_join(value_counts_parsimony_glottolog) %>%
  full_join(value_counts_parsimony_gray) 

rm(value_counts_ML_gray, value_counts_ML_glottolog, value_counts_parsimony_glottolog, value_counts_parsimony_gray)

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

#Function for getting ancestral states for 4 specific nodes out of raydisc gray version
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

##Gray tree ML 
GB_ASR_RDS_ML_gray <- readRDS("output/gray_et_al_2009/ML/GB_ML_gray_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_ML_gray))

df_lik_anc_ML_gray <- lapply(GB_ASR_RDS_ML_gray$content, get_node_positions) %>% bind_rows()

df_lik_anc_ML_gray$gray_ML_prediction <- if_else(df_lik_anc_ML_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_ML_gray <- df_lik_anc_ML_gray %>% 
  mutate(`0` = round(`0` ,2)) %>% 
  mutate(`1` = round(`1` ,2)) %>% 
  dplyr::select(`Grambank ID`, "Proto-language", gray_ML_prediction,gray_ML_prediction_0 = `0`, gray_ML_prediction_1 = `1`)



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



###Gray parsimony
GB_ACR_all_parsimony <- readRDS("output/gray_et_al_2009/parsimony/GB_parsimony_gray_tree.rds")

df_lik_anc_parsimony_gray <- lapply(GB_ACR_all_parsimony$content, get_node_positions_parsimony) %>% bind_rows()

df_lik_anc_parsimony_gray$Parsimony_gray_prediction <- if_else(df_lik_anc_parsimony_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_gray$gray_parsimony_prediction <- if_else(df_lik_anc_parsimony_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_gray <- df_lik_anc_parsimony_gray %>% 
  mutate(`0` = round(`0`)) %>% 
  mutate(`1` = round(`1`)) %>% 
  dplyr::select(`Grambank ID`, "Proto-language", gray_parsimony_prediction,gray_parsimony_prediction_0 = `0`, gray_parsimony_prediction_1 = `1`)

cat("Done with retreiving the particular states for 4 proto-languages for all features, each tree and each method.")

##comparing gray and glottolog

automatic_predctions <- df_lik_anc_ML_glottolog  %>% 
  full_join(df_lik_anc_ML_gray) %>% 
  full_join(df_lik_anc_parsimony_gray) %>% 
  full_join(df_lik_anc_parsimony_glottolog)

df <- HL_findings_sheet %>% 
  left_join(automatic_predctions) 

df$`ML result (Glottolog-tree)` <- if_else(df$glottolog_ML_prediction == "Present" & df$Prediction == "Present", "True Positive",  
                                           if_else(df$glottolog_ML_prediction == "Absent" & df$Prediction == "Absent", "True Negative",   
                                                   if_else(df$glottolog_ML_prediction == "Absent" & df$Prediction == "Present", "False Negative",  
                                                           if_else(df$glottolog_ML_prediction == "Present" & df$Prediction == "Absent", "False Positive",
                                                                   
                                                                   ifelse(df$glottolog_ML_prediction == "Half", "Half", NA)))))


df$`ML result (Gray et al 2009-tree)` <- if_else(df$gray_ML_prediction == "Present" & df$Prediction == "Present", "True Positive",  
                                                 if_else(df$gray_ML_prediction == "Absent" & df$Prediction == "Absent", "True Negative",   
                                                         if_else(df$gray_ML_prediction == "Absent" & df$Prediction == "Present", "False Negative",  
                                                                 if_else(df$gray_ML_prediction == "Present" & df$Prediction == "Absent", "False Positive",
                                                                         
                                                                         ifelse(df$gray_ML_prediction == "Half", "Half", NA)))))


df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$gray_parsimony_prediction == "Present" & df$Prediction == "Present", "True Positive",  
                                                 if_else(df$gray_parsimony_prediction == "Absent" & df$Prediction == "Absent", "True Negative",   
                                                         if_else(df$gray_parsimony_prediction == "Absent" & df$Prediction == "Present", "False Negative",  
                                                                 if_else(df$gray_parsimony_prediction == "Present" & df$Prediction == "Absent", "False Positive",
                                                                         
                                                                         ifelse(df$gray_parsimony_prediction == "Half", "Half", NA)))))


df$`Parsimony result (Glottolog-tree)` <- if_else(df$glottolog_parsimony_prediction == "Present" & df$Prediction == "Present", "True Positive",  
                                                        if_else(df$glottolog_parsimony_prediction == "Absent" & df$Prediction == "Absent", "True Negative",   
                                                                if_else(df$glottolog_parsimony_prediction == "Absent" & df$Prediction == "Present", "False Negative",  
                                                                        if_else(df$glottolog_parsimony_prediction == "Present" & df$Prediction == "Absent", "False Positive",
                                                                                
                                                                                ifelse(df$glottolog_parsimony_prediction == "Half", "Half", NA)))))

df <- value_count_df %>% 
  rename(`Grambank ID` = Feature_ID) %>% 
  right_join(df)

df$`ML result (Gray et al 2009-tree)`<- if_else(df$ntips_ML_gray <  62, "Not enough languages", df$`ML result (Gray et al 2009-tree)`)
df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$ntips_parsimony_gray <  62, "Not enough languages", df$`Parsimony result (Gray et al 2009-tree)`)
df$`ML result (Glottolog-tree)` <- if_else(df$ntips_ML_glottolog <  117, "Not enough languages", df$`ML result (Glottolog-tree)` )
df$`Parsimony result (Glottolog-tree)` <- if_else(df$ntips_parsimony_glottolog <  117, "Not enough languages", df$`Parsimony result (Glottolog-tree)`)

#Counting up trues per feature
df$countTruePos <- rowSums(df == "True Positive", na.rm = T)

df$countTrueNeg <- rowSums(df == "True Negative", na.rm = T)

df$countTrue <- df$countTruePos + df$countTrueNeg

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
 dplyr::select(`Grambank ID` = ID, Abbreviation =Grambank_ID_desc, Question = Name) 


df_pruned_erg <- df %>% 
  arrange(`Proto-language`, `Grambank ID`) %>% 
  filter(`Grambank ID` == "GB409" |
    `Grambank ID` == "GB408" |
      `Grambank ID` == "GB410" |
          `Grambank ID` == "GB147" )   %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Grambank ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select(`Grambank ID` = `Grambank ID`, Abbreviation, Question, "Proto-language", `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` ,
                `Parsimony result (Gray et al 2009-tree)`,
                `ML result (Gray et al 2009-tree)`)

df_pruned_erg %>% 
  write_tsv("output/HL_comparison/HL_comparison_erg.tsv")


df_non_erg <- df %>% 
  anti_join(df_pruned_erg) %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Grambank ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select(`Grambank ID` = `Grambank ID`, Abbreviation, Question, "Proto-language",  `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` ,
                `Parsimony result (Gray et al 2009-tree)`,
                `ML result (Gray et al 2009-tree)`)


df_non_erg %>% 
  write_tsv("output/HL_comparison/HL_comparison.tsv")

##

#ntips_ML_glottolog,
#ntips_ML_gray,
#ntips_parsimony_gray, 
#ntips_parsimony_glottolog,

accuracy_summary_table<- df %>% 
  anti_join(df_pruned_erg) %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Grambank ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select("Grambank ID" , 
                `Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` ,
                `Parsimony result (Gray et al 2009-tree)`,
                `ML result (Gray et al 2009-tree)`) %>%
  reshape2::melt(id.vars = "Grambank ID" ) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  reshape2::dcast(variable ~ value, value.var = "n") %>% 
  group_by(variable) %>% 
  mutate(Disagree = sum(`False Negative`, `False Positive`),
                        Agree = sum(`True Negative` , `True Positive`), 
         reconstructions_non_half = sum(Agree, Disagree),
         reconstructions_all= sum(Agree, Disagree, Half)) %>% 
  mutate(Accuracy = Agree / reconstructions_non_half, 
         Accuracy_incl_half = ((Agree + (Half/2)) / reconstructions_all), 
         Precision = `True Positive` / (`True Positive` + `False Positive`), 
         Recall = `True Positive` / (`True Positive` + `False Negative`)) %>%
  mutate(F1_score = 2 * ((Precision*Recall)/(Precision + Recall))) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  t() 
  
colnames(accuracy_summary_table) <- accuracy_summary_table[1,]    
accuracy_summary_table <- accuracy_summary_table[-1,]    

accuracy_summary_table %>% 
  as.data.frame() %>% 
  rownames_to_column("Stat") %>% 
write_tsv("output/HL_comparison/HL_comparison_summary.tsv")