source("01_requirements.R")
source("fun_get_ASR_nodes_parsimony.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets
HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with

value_count_df <- read_csv("output/gray_et_al_2009/parsimony/mcct/results.csv") %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_gray = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_gray = ntips, zeroes_parsimony_gray = `0`, ones_parsimony_gray = `1`, min_percent_parsimony_gray)
  
#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

##########################################  ML ##########################################

#Function for getting ancestral states for 4 specific nodes out of corHMM gray version
get_node_positions_ML <- function(GB_asr_object_ml){

#GB_asr_object_ml <- GB_ASR_RDS_ML_gray$content[[1]]

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

df_lik_anc_ML_gray <- lapply(GB_ASR_RDS_ML_gray$content, get_node_positions_ML) %>% bind_rows()

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

df_lik_anc_ML_glottolog <- lapply(GB_ASR_RDS_ML_glottolog$content, get_node_positions_ML) %>% bind_rows()

df_lik_anc_ML_glottolog$glottolog_ML_prediction <- if_else(df_lik_anc_ML_glottolog$`0` > 0.6, "Absent", if_else(df_lik_anc_ML_glottolog$`1` > 0.6, "Present", "Half")) 

df_lik_anc_ML_glottolog <- df_lik_anc_ML_glottolog %>% 
  mutate(`0` = round(`0` ,2)) %>% 
  mutate(`1` = round(`1` ,2)) %>% 
  dplyr::select(Feature_ID, "Proto-language", glottolog_ML_prediction,glottolog_ML_prediction_0 = `0`, glottolog_ML_prediction_1 = `1`)

########################################## SCM ##########################################
get_node_positions_SCM <- function(GB_asr_object_SCM){
  
  #GB_asr_object_SCM <- GB_ASR_RDS_SCM_gray$content[[1]]

  feature <- GB_asr_object_SCM[[2]][1,1]
  tree <- GB_asr_object_SCM[[1]]
  
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

  df_lik_anc <-   tree$mapped.edge %>% as.data.frame() %>% View()
  df_lik_anc$Node <- seq(Ntip(tree) + 1, Ntip(tree) + nrow(df_lik_anc))   # i.e. count from ntips + 1 …to .. ntips + number of nodes
  df <- df_proto_nodes %>% 
    left_join(df_lik_anc, by = "Node") %>% 
    mutate(Feature_ID = feature) %>% 
    rename(`0`= "(1,R1)" , `1` = "(2,R1)")
  
  cat("I'm done with finding the SCM proto-language states for feature ", feature, ".\n", sep = "")
  df
}

##Gray scm
GB_ASR_RDS_SCM_gray <- readRDS("output/gray_et_al_2009/SCM/mcct/GB_SCM_gray_tree.rds") %>% 
  left_join(value_count_df, by = "Feature_ID") %>% 
  filter(!is.na(ntips_SCM_gray))

df_lik_anc_SCM_gray <- lapply(GB_ASR_RDS_SCM_gray$content, get_node_positions_ML) %>% bind_rows()




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

##Marking which results can't be included because they don't have enough languages

#ML Gray
df$`ML result (Gray et al 2009-tree)` <- if_else(df$ntips_ML_gray <  ntips_half_gray, "Not enough languages", df$`ML result (Gray et al 2009-tree)`)
df$gray_ML_prediction <- if_else(df$ntips_ML_gray <  ntips_half_gray, "Not enough languages", df$gray_ML_prediction)
df$gray_ML_prediction_1 <- ifelse(df$ntips_ML_gray <  ntips_half_gray, NA, df$gray_ML_prediction_1)
df$gray_ML_prediction_0 <- ifelse(df$ntips_ML_gray <  ntips_half_gray, NA, df$gray_ML_prediction_0)


#Parsimony gray
df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", df$`Parsimony result (Gray et al 2009-tree)`)
df$gray_parsimony_prediction <- if_else(df$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", df$gray_parsimony_prediction)
df$gray_parsimony_prediction_1 <- ifelse(df$ntips_parsimony_gray <  ntips_half_gray, NA, df$gray_parsimony_prediction_1)
df$gray_parsimony_prediction_0 <- ifelse(df$ntips_parsimony_gray <  ntips_half_gray, NA, df$gray_parsimony_prediction_0)

#ML Glottolog
df$`ML result (Glottolog-tree)` <- if_else(df$ntips_ML_glottolog <  ntips_half_glottolog, "Not enough languages", df$`ML result (Glottolog-tree)` )
df$glottolog_ML_prediction <- if_else(df$ntips_ML_glottolog <  ntips_half_glottolog, "Not enough languages", df$glottolog_ML_prediction)
df$glottolog_ML_prediction_1 <- ifelse(df$ntips_ML_glottolog <  ntips_half_glottolog, NA, df$glottolog_ML_prediction_1 )
df$glottolog_ML_prediction_0 <- ifelse(df$ntips_ML_glottolog <  ntips_half_glottolog, NA, df$glottolog_ML_prediction_0 )


#parsimony glottolog
df$`Parsimony result (Glottolog-tree)` <- if_else(df$ntips_parsimony_glottolog <  ntips_half_glottolog, "Not enough languages", df$`Parsimony result (Glottolog-tree)`)
df$glottolog_parsimony_prediction <- if_else(df$ntips_parsimony_glottolog <  ntips_half_glottolog, "Not enough languages", df$glottolog_parsimony_prediction)
df$glottolog_parsimony_prediction_1 <- ifelse(df$ntips_parsimony_glottolog <  ntips_half_glottolog, NA, df$glottolog_parsimony_prediction_1)
df$glottolog_parsimony_prediction_0 <- ifelse(df$ntips_parsimony_glottolog <  ntips_half_glottolog, NA, df$glottolog_parsimony_prediction_0)


#Counting up trues per feature
df$countTruePos <- rowSums(df == "True Positive", na.rm = T)

df$countTrueNeg <- rowSums(df == "True Negative", na.rm = T)

df$countTrue <- df$countTruePos + df$countTrueNeg

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 

df %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  write_tsv("output/HL_comparison/mcct/all_reconstructions.tsv")  

df_non_erg <- df %>% 
  filter(!is.na(Prediction)) %>% 
  arrange(-countTrue) %>% 
  left_join(GB_df_desc) %>% 
  separate(Abbreviation, into = c("Feature_ID_abbreviation", "Abbreviation"), sep = " ") %>% 
  dplyr::select(Feature_ID, Abbreviation, Question, "Proto-language",  `Finding from Historical Linguistics` = Prediction, "Historical Linguistics sources" ,`Parsimony result (Glottolog-tree)`,
                `ML result (Glottolog-tree)` ,
                `Parsimony result (Gray et al 2009-tree)`,
                `ML result (Gray et al 2009-tree)`)


df_non_erg %>% 
  write_tsv("output/HL_comparison/mcct/HL_comparison.tsv")



