source("01_requirements.R")



#reading in old sheet with HL-predictions
HL_findings_sheet <- read_tsv("data/HL_findings/GB_sheets/HS_cent2060.tsv") %>% 
  mutate("Proto-language" = "Proto-Central Pacific",
         Language_ID = "cent2060") %>% 
  full_join(read_tsv("data/HL_findings/GB_sheets/HS_east2449.tsv") %>% 
              mutate("Proto-language" = "Proto-Eastern Polynesian",
                     Language_ID =  "east2449")) %>% 
  full_join(read_tsv("data/HL_findings/GB_sheets/HS_poly1242.tsv") %>% 
            mutate("Proto-language" = "Proto-Polynesian",
                     Language_ID = "poly1242")
            ) %>% 
full_join(read_tsv("data/HL_findings/GB_sheets/HS_ocea1241.tsv")%>% 
            mutate("Proto-language" = "Proto-Oceanic",
                   Language_ID =   "ocea1241")) %>% 
  full_join(read_csv("data/HL_findings/HL_findings_binarised_extra.csv")) %>% 
  dplyr::select(Feature_ID, "Proto-language", Language_ID, Prediction = Value, "Historical Linguistics sources" = "Source (Latex)") 

###Make coding sheets binarised


#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
if("GB024" %in% colnames(HL_findings_sheet)){
  HL_findings_sheet$GB024a <- if_else(HL_findings_sheet$GB024 == "1"|HL_findings_sheet$GB024 == "3", "1", ifelse(HL_findings_sheet$GB024 == "2", "0", NA)) 
  
  HL_findings_sheet$GB024b <- if_else(HL_findings_sheet$GB024 == "2"|HL_findings_sheet$GB024 == "3", "1", ifelse(HL_findings_sheet$GB024 == "1", "0", NA)) 
}

#GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
if("GB025" %in% colnames(HL_findings_sheet)){
  HL_findings_sheet$GB025a <- if_else(HL_findings_sheet$GB025 == "1"|HL_findings_sheet$GB025 == "3", "1", ifelse(HL_findings_sheet$GB025 == "2", "0", NA)) 
  
  HL_findings_sheet$GB025b <- ifelse(HL_findings_sheet$GB025 == "2"|HL_findings_sheet$GB025 == "3", "1", ifelse(HL_findings_sheet$GB025 == "1", "0", NA)) 
}

#GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
if("GB065" %in% colnames(HL_findings_sheet)){
  HL_findings_sheet$GB065a <- if_else(HL_findings_sheet$GB065 == "1"|HL_findings_sheet$GB065 == "3", "1", ifelse(HL_findings_sheet$GB065 == "2", "0", NA)) 
  
  HL_findings_sheet$GB065b <- if_else(HL_findings_sheet$GB065 == "2"|HL_findings_sheet$GB065 == "3", "1", ifelse(HL_findings_sheet$GB065 == "1", "0", NA)) 
}

#GB130 multistate 1: SV; 2: VS; 3: both
if("GB130" %in% colnames(HL_findings_sheet)){
  HL_findings_sheet$GB130a <- if_else(HL_findings_sheet$GB130 == "1"|HL_findings_sheet$GB130 == "3", "1", ifelse(HL_findings_sheet$GB130 == "2", "0", NA)) 
  
  HL_findings_sheet$GB130b <- if_else(HL_findings_sheet$GB130 == "2"|HL_findings_sheet$GB130 == "3", "1", ifelse(HL_findings_sheet$GB130 == "1", "0", NA)) 
}

#GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
if("GB193" %in% colnames(HL_findings_sheet)){
  HL_findings_sheet$GB193a <- if_else(HL_findings_sheet$GB193 == "1"|HL_findings_sheet$GB193 == "3", "1", ifelse(HL_findings_sheet$GB193 == "2"|HL_findings_sheet$GB193 == "0", "0", NA)) 
  
  HL_findings_sheet$GB193b <- if_else(HL_findings_sheet$GB193 == "2"|HL_findings_sheet$GB193 == "3", "1", ifelse(HL_findings_sheet$GB193 == "1"|HL_findings_sheet$GB193 == "0", "0", NA)) 
}
#GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.
if("GB203" %in% colnames(HL_findings_sheet)){
  HL_findings_sheet$GB203a <- if_else(HL_findings_sheet$GB203 == "1"|HL_findings_sheet$GB203 == "3", "1", ifelse(HL_findings_sheet$GB203 == "2"|HL_findings_sheet$GB203 == "0", "0", NA)) 
  
  HL_findings_sheet$GB203b <- if_else(HL_findings_sheet$GB203 == "2"|HL_findings_sheet$GB203 == "3", "1", ifelse(HL_findings_sheet$GB203 == "1"|HL_findings_sheet$GB203 == "0", "0", NA)) 
}


####

#reading in old sheet with HL-predictions - ergative section
HL_findings_sheet_conflict <- read_csv("data/HL_findings/HL_findings_conflicts.csv")%>% 
  
  dplyr::select(Feature_ID, "Proto-language", Language_ID, Prediction = Value, "Historical Linguistics sources" = "Source (Latex)") 

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with

value_counts_parsimony_glottolog <- read_csv("output/glottolog_tree_binary/parsimony/results.csv") %>%
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_glottolog = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_glottolog = ntips, zeroes_parsimony_glottolog = `0`, ones_parsimony_glottolog = `1`, min_percent_parsimony_glottolog) 

value_counts_parsimony_gray <- read_csv("output/gray_et_al_2009/parsimony/mcct/results.csv") %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_gray = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_gray = ntips, zeroes_parsimony_gray = `0`, ones_parsimony_gray = `1`, min_percent_parsimony_gray)

value_counts_ML_glottolog <- read_csv("output/glottolog_tree_binary/ML/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_glottolog = min / nTips) %>%
  dplyr::select(Feature_ID, ntips_ML_glottolog = nTips, zeroes_ML_glottolog =  nTips_state_0, ones_ML_glottolog =  nTips_state_1, min_percent_ML_glottolog)

value_counts_ML_gray <- read_csv("output/gray_et_al_2009//ML/mcct/results.csv") %>%
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
  left_join(df_lik_anc, by = "Node") %>% 
  mutate(Feature_ID = feature) %>% 
  rename(`0`= "(1,R1)" , `1` = "(2,R1)")

cat("I'm done with finding the ML proto-language states for feature ", feature, ".\n", sep = "")
df
}

##Gray tree ML 
GB_ASR_RDS_ML_gray <- readRDS("output/gray_et_al_2009/ML/mcct/GB_ML_gray_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_ML_gray))

df_lik_anc_ML_gray <- lapply(GB_ASR_RDS_ML_gray$content, get_node_positions) %>% bind_rows()

df_lik_anc_ML_gray$gray_ML_prediction <- if_else(df_lik_anc_ML_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_ML_gray <- df_lik_anc_ML_gray %>% 
  mutate(`0` = round(`0` ,2)) %>% 
  mutate(`1` = round(`1` ,2)) %>% 
  dplyr::select(Feature_ID, "Proto-language", gray_ML_prediction,gray_ML_prediction_0 = `0`, gray_ML_prediction_1 = `1`)



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
  dplyr::select(Feature_ID, "Proto-language", glottolog_ML_prediction,glottolog_ML_prediction_0 = `0`, glottolog_ML_prediction_1 = `1`)


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
    mutate(Feature_ID = feature)
  
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
  dplyr::select(Feature_ID, "Proto-language", glottolog_parsimony_prediction,glottolog_parsimony_prediction_0 = `0`, glottolog_parsimony_prediction_1 = `1`)



###Gray parsimony
GB_ACR_all_parsimony <- readRDS("output/gray_et_al_2009/parsimony/mcct/GB_parsimony_gray_tree.rds")

df_lik_anc_parsimony_gray <- lapply(GB_ACR_all_parsimony$content, get_node_positions_parsimony) %>% bind_rows()

df_lik_anc_parsimony_gray$Parsimony_gray_prediction <- if_else(df_lik_anc_parsimony_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_gray$gray_parsimony_prediction <- if_else(df_lik_anc_parsimony_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_gray <- df_lik_anc_parsimony_gray %>% 
  mutate(`0` = round(`0`)) %>% 
  mutate(`1` = round(`1`)) %>% 
  dplyr::select(Feature_ID, "Proto-language", gray_parsimony_prediction,gray_parsimony_prediction_0 = `0`, gray_parsimony_prediction_1 = `1`)

cat("Done with retreiving the particular states for 4 proto-languages for all features, each tree and each method.")

##############################################################################
##comparing gray and glottolog

automatic_predctions <- df_lik_anc_ML_glottolog  %>% 
  full_join(df_lik_anc_ML_gray, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(df_lik_anc_parsimony_gray, by = c("Feature_ID", "Proto-language")) %>% 
  full_join(df_lik_anc_parsimony_glottolog, by = c("Feature_ID", "Proto-language"))

df <- HL_findings_sheet %>% 
  right_join(automatic_predctions, by = c("Feature_ID", "Proto-language")) 

##Marking more clearly results
df$`ML result (Glottolog-tree)` <- if_else(df$glottolog_ML_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                           if_else(df$glottolog_ML_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                   if_else(df$glottolog_ML_prediction == "Absent" & 
                                                             df$Prediction == 1, "False Negative",  
                                                           if_else(df$glottolog_ML_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                   
                                                                   ifelse(df$glottolog_ML_prediction == "Half", "Half", NA)))))

df$`ML result (Gray et al 2009-tree)` <- if_else(df$gray_ML_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                 if_else(df$gray_ML_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                         if_else(df$gray_ML_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                 if_else(df$gray_ML_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                         
                                                                         ifelse(df$gray_ML_prediction == "Half", "Half", NA)))))


df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$gray_parsimony_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                 if_else(df$gray_parsimony_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                         if_else(df$gray_parsimony_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                 if_else(df$gray_parsimony_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                         
                                                                         ifelse(df$gray_parsimony_prediction == "Half", "Half", NA)))))


df$`Parsimony result (Glottolog-tree)` <- if_else(df$glottolog_parsimony_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                        if_else(df$glottolog_parsimony_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                                if_else(df$glottolog_parsimony_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                        if_else(df$glottolog_parsimony_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                                
                                                                                ifelse(df$glottolog_parsimony_prediction == "Half", "Half", NA)))))

df <- value_count_df %>% 
  right_join(df) 

df$`ML result (Gray et al 2009-tree)`<- if_else(df$ntips_ML_gray <  ntips_half_gray, "Not enough languages", df$`ML result (Gray et al 2009-tree)`)
df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", df$`Parsimony result (Gray et al 2009-tree)`)
df$`ML result (Glottolog-tree)` <- if_else(df$ntips_ML_glottolog <  ntips_half_glottolog, "Not enough languages", df$`ML result (Glottolog-tree)` )
df$`Parsimony result (Glottolog-tree)` <- if_else(df$ntips_parsimony_glottolog <  ntips_half_glottolog, "Not enough languages", df$`Parsimony result (Glottolog-tree)`)

#Counting up trues per feature
df$countTruePos <- rowSums(df == "True Positive", na.rm = T)

df$countTrueNeg <- rowSums(df == "True Negative", na.rm = T)

df$countTrue <- df$countTruePos + df$countTrueNeg

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
 dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df_non_erg <- df %>% 
  filter(!is.na(Prediction)) %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Feature_ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select(Feature_ID, Abbreviation, Question, "Proto-language",  `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
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
  filter(!is.na(Prediction)) %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("`Feature_ID`", "Abbreviation"), sep = " ") %>% 
  dplyr::select("Feature_ID" , 
                `Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` ,
                `Parsimony result (Gray et al 2009-tree)`,
                `ML result (Gray et al 2009-tree)`) %>%
  reshape2::melt(id.vars = "Feature_ID" ) %>% 
  filter(!is.na(value)) %>% 
  group_by(variable, value) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  reshape2::dcast(variable ~ value, value.var = "n") %>% 
  group_by(variable) %>% 
  mutate(Disagree = sum(`False Negative`, `False Positive`, na.rm = T),
                        Agree = sum(`True Negative` , `True Positive`, na.rm = T), 
         reconstructions_non_half = sum(Agree, Disagree, na.rm = T),
         reconstructions_all= sum(Agree, Disagree, Half, na.rm = T)) %>% 
  mutate(Accuracy = Agree / reconstructions_non_half, 
         Accuracy_incl_half = ((Agree + (Half/2)) / reconstructions_all), 
         Precision = `True Positive` / (`True Positive` + `False Positive`), 
         Recall = `True Positive` / (`True Positive` + `False Negative`)) %>%
  mutate(F1_score = 2 * ((Precision*Recall)/(Precision + Recall))) %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  t() 
  
colnames(accuracy_summary_table) <- accuracy_summary_table[1,]    
accuracy_summary_table <- accuracy_summary_table[-1,]    

accuracy_summary_table %>% 
  as.data.frame() %>% 
  rownames_to_column("Stat") %>% 
write_tsv("output/HL_comparison/HL_comparison_summary_mcct.tsv")

##new predictions

df_new_preductions_positive <-  df %>%
  filter(is.na(Prediction)) %>% 
  filter(glottolog_ML_prediction == "Present") %>% 
  filter(glottolog_parsimony_prediction == "Present") %>% 
  filter(gray_ML_prediction == "Present") %>% 
  filter(gray_parsimony_prediction == "Present") %>%
  filter(min_percent_ML_glottolog > 0.3) %>% 
  dplyr::select(Feature_ID, `Proto-language`, automatic_prediction = gray_parsimony_prediction) %>% 
  left_join(GB_df_desc) 
  

##CONFLICTS

df_conflict_resolution <- HL_findings_sheet_conflict %>% 
  full_join(automatic_predctions, by = c("Feature_ID", "Proto-language")) 

# 
# df_pruned_erg <- df %>% 
#   filter(!is.na(Prediction)) %>% 
#   arrange(`Proto-language`, Feature_ID) %>% 
#   filter(Feature_ID == "GB409" |
#            Feature_ID == "GB408" ) %>% 
#   #      `Feature_ID` == "GB410" |
#   #     `Feature_ID` == "GB147" )   %>% 
#   left_join(GB_df_desc) %>% 
#   separate(Abbreviation, into = c("Feature_ID", "Abbreviation"), sep = " ") %>% 
#   dplyr::select(Feature_ID, Abbreviation, Question, "Proto-language", `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
#                 `ML result (Glottolog-tree)` ,
#                 `Parsimony result (Gray et al 2009-tree)`,
#                 `ML result (Gray et al 2009-tree)`)
# 
# df_pruned_erg %>% 
#   write_tsv("output/HL_comparison/HL_comparison_erg.tsv")

