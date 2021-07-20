source("01_requirements.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets

HL_findings_sheet <- read_tsv("data/GB/grambank-cldf/raw/Grambank/original_sheets/HS_cent2060.tsv", col_types = cols(.default = "c")) %>% 
  mutate("Proto-language" = "Proto-Central Pacific",
         Language_ID = "cent2060") %>% 
  full_join(read_tsv("data/GB/grambank-cldf/raw/Grambank/original_sheets/HS_east2449.tsv", col_types = cols(.default = "c")) %>% 
              mutate("Proto-language" = "Proto-Eastern Polynesian",
                     Language_ID =  "east2449"),
            by = c("Feature_ID", "Feature", "Value", "Source (Latex)", "Source", "Comment", "Proto-language", "Language_ID")) %>% 
  full_join(read_tsv("data/GB/grambank-cldf/raw/Grambank/original_sheets/HS_poly1242.tsv", col_types = cols(.default = "c")) %>% 
              mutate("Proto-language" = "Proto-Polynesian",
                     Language_ID = "poly1242"),
            by = c("Feature_ID", "Feature", "Value", "Source (Latex)", "Source", "Comment", "Proto-language", "Language_ID")  ) %>% 
  full_join(read_tsv("data/GB/grambank-cldf/raw/Grambank/original_sheets/HS_ocea1241.tsv", col_types = cols(.default = "c"))%>% 
              mutate("Proto-language" = "Proto-Oceanic",
                     Language_ID =   "ocea1241"),
            by = c("Feature_ID", "Feature", "Value", "Source (Latex)", "Source", "Comment", "Proto-language", "Language_ID")) %>% 
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

#glottolog df information with branch names, so that we can easily subset for the different groups based on "classification"
#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, Name)

#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc, Question = Name) 
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
  
  cat("I'm done with finding the parsimony proto-language states for feature ", feature, " for ", dir, ".\n", sep = "")
  
  df
}


dirs <- list.dirs("output/gray_et_al_2009/parsimony/results_by_tree/", recursive = F)

for(dir in dirs){
#start

#  dir <- dirs[1]  

  
  ##creating dfs which show the number of tips per tree per method, as well as the general distribution at the tips. This makes it possible for us for example to exclude results with too few tips. We'll use this df later to filter with
  
  value_count_df <- read_csv(file.path(dir, "results.csv"), col_types = 
                               cols(
                                    Parsimony_cost = col_double(),
                                    `0` = col_double(),
                                    `1` = col_double(),
                                    Feature_ID = col_character(),
                                    ntips = col_double()
  )) %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_gray = min / (`0`+ `1`)) %>%
  dplyr::select(Feature_ID, ntips_parsimony_gray = ntips, zeroes_parsimony_gray = `0`, ones_parsimony_gray = `1`, min_percent_parsimony_gray)



###Gray parsimony
GB_ACR_all_parsimony <- readRDS(file.path(dir, "GB_parsimony_gray_tree.rds"))

df_lik_anc_parsimony_gray <- lapply(GB_ACR_all_parsimony$content, get_node_positions_parsimony) %>% bind_rows()

df_lik_anc_parsimony_gray$gray_parsimony_prediction <- if_else(df_lik_anc_parsimony_gray$`0` > 0.6, "Absent", if_else(df_lik_anc_parsimony_gray$`1` > 0.6, "Present", "Half")) 

df_lik_anc_parsimony_gray <- df_lik_anc_parsimony_gray %>% 
  mutate(`0` = round(`0`)) %>% 
  mutate(`1` = round(`1`)) %>% 
  dplyr::select(Feature_ID, "Proto-language", gray_parsimony_prediction,gray_parsimony_prediction_0 = `0`, gray_parsimony_prediction_1 = `1`)

df <- HL_findings_sheet %>% 
  right_join(df_lik_anc_parsimony_gray, by = c("Feature_ID", "Proto-language")) 


df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$gray_parsimony_prediction == "Present" & df$Prediction == 1, "True Positive",  
                                                        if_else(df$gray_parsimony_prediction == "Absent" & df$Prediction == 0, "True Negative",   
                                                                if_else(df$gray_parsimony_prediction == "Absent" & df$Prediction == 1, "False Negative",  
                                                                        if_else(df$gray_parsimony_prediction == "Present" & df$Prediction == 0, "False Positive",
                                                                                
                                                                                ifelse(df$gray_parsimony_prediction == "Half", "Half", NA)))))


df <- value_count_df %>% 
  right_join(df, by = "Feature_ID") 

##Marking which results can't be included because they don't have enough languages

#Parsimony gray
df$`Parsimony result (Gray et al 2009-tree)` <- if_else(df$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", df$`Parsimony result (Gray et al 2009-tree)`)
df$gray_parsimony_prediction <- if_else(df$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", df$gray_parsimony_prediction)
df$gray_parsimony_prediction_1 <- ifelse(df$ntips_parsimony_gray <  ntips_half_gray, NA, df$gray_parsimony_prediction_1)
df$gray_parsimony_prediction_0 <- ifelse(df$ntips_parsimony_gray <  ntips_half_gray, NA, df$gray_parsimony_prediction_0)

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