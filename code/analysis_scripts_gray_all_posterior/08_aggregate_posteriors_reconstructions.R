source("01_requirements.R")

#reading in HL predictions
HL_findings_sheet_conflicts <- read_csv(HL_findings_sheet_conflicts_fn ) %>% 
  mutate(conflict = "Yes") %>% 
  rename(Prediction = Value)

HL_findings_sheets <-read_tsv(HL_findings_sheet_fn, show_col_types = F)   %>% 
  full_join(HL_findings_sheet_conflicts, by = c("Proto-language", "Feature_ID", "Prediction")) %>% 
  dplyr::select("Proto-language", "Feature_ID", "Prediction")

################ parsimony #####################

#aggregating the parsimony conclusions
fns <- list.files("output/gray_et_al_2009/parsimony/results_by_tree/", recursive = T, pattern = "*all_reconstructions.tsv", full.names = T)

all_parsimony_reconstructions_df <- fns %>% 
  map_df(
    function(x) data.table::fread(x ,
                                  encoding = 'UTF-8', header = TRUE, 
                                  fill = TRUE, blank.lines.skip = TRUE,
                                  sep = "\t", na.strings = "NA",
    )   %>% 
      dplyr::select(Feature_ID, 
                    gray_parsimony_prediction_0, 
                    gray_parsimony_prediction_1, 
                    ntips_parsimony_gray, 
                    `Proto-language`, 
                    min_percent_parsimony_gray, 
                    min_parsimony_gray) %>% 
      mutate(gray_parsimony_prediction_0 = as.numeric(gray_parsimony_prediction_0)) %>% 
      mutate(gray_parsimony_prediction_1 = as.numeric(gray_parsimony_prediction_1),
             ntips_parsimony_gray = as.numeric(ntips_parsimony_gray), 
             min_parsimony_gray = as.numeric(min_parsimony_gray),
             min_percent_parsimony_gray = as.numeric(min_percent_parsimony_gray)) %>% 
      
      mutate(filename = x) 
  ) 

all_parsimony_reconstructions_df_summarised <-  all_parsimony_reconstructions_df %>% 
  group_by(Feature_ID, `Proto-language`) %>% 
  summarise(gray_parsimony_prediction_0 = mean(gray_parsimony_prediction_0, na.rm = T),
            gray_parsimony_prediction_1 = mean(gray_parsimony_prediction_1, na.rm = T), 
            ntips_parsimony_gray = mean(ntips_parsimony_gray, na.rm = T),
            min_parsimony_gray = mean(min_parsimony_gray),
            min_percent_parsimony_gray = mean( min_percent_parsimony_gray ), .groups = "drop") 

#summarising prediction into present or absent, or half-half
all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction <- if_else(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction_0 > 0.6, "Absent", if_else(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction_1 > 0.6, "Present", "Half")) 

#add in HL findings
all_parsimony_reconstructions_df_summarised <- all_parsimony_reconstructions_df_summarised %>% 
  left_join(HL_findings_sheets,  by = c("Feature_ID", "Proto-language"))

#calculate the concordance with HL
all_parsimony_reconstructions_df_summarised$`Parsimony result (Gray et al 2009-tree posteriors)` <- if_else(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction == "Present" & all_parsimony_reconstructions_df_summarised$Prediction == 1, "True Positive",  
                                                        if_else(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction == "Absent" & all_parsimony_reconstructions_df_summarised$Prediction == 0, "True Negative",   
                                                                if_else(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction == "Absent" & all_parsimony_reconstructions_df_summarised$Prediction == 1, "False Negative",  
                                                                        if_else(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction == "Present" & all_parsimony_reconstructions_df_summarised$Prediction == 0, "False Positive",
                                                                                
                                                                                ifelse(all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction == "Half", "Half", NA)))))

##Marking which results can't be included because they don't have enough languages

#Parsimony gray
all_parsimony_reconstructions_df_summarised$`Parsimony result (Gray et al 2009-tree posteriors)`<- if_else(all_parsimony_reconstructions_df_summarised$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", all_parsimony_reconstructions_df_summarised$`Parsimony result (Gray et al 2009-tree posteriors)`)

all_parsimony_reconstructions_df_summarised$`Parsimony result (Gray et al 2009-tree posteriors)` <- ifelse(is.na(all_parsimony_reconstructions_df_summarised$Prediction), NA, all_parsimony_reconstructions_df_summarised$`Parsimony result (Gray et al 2009-tree posteriors)`)

all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction <- if_else(all_parsimony_reconstructions_df_summarised$ntips_parsimony_gray <  ntips_half_gray, "Not enough languages", all_parsimony_reconstructions_df_summarised$gray_parsimony_prediction)

all_parsimony_reconstructions_df_summarised %>%
  write_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")


############## ML ##############



#aggregating the ML conclusions
fns <- list.files("output/gray_et_al_2009/ML/results_by_tree/", recursive = T, pattern = "*all_reconstructions.tsv", full.names = T)

all_ML_reconstructions_df <- fns %>% 
  map_df(
    function(x) data.table::fread(x ,
                                  encoding = 'UTF-8', header = TRUE, 
                                  fill = TRUE, blank.lines.skip = TRUE,
                                  sep = "\t", na.strings = "NA",
    )   %>% 
      dplyr::select(Feature_ID, gray_ML_prediction_0, gray_ML_prediction_1, ntips_ML_gray, `Proto-language`, min_ML_gray, min_percent_ML_gray) %>% 
      mutate(gray_ML_prediction_0 = as.numeric(gray_ML_prediction_0)) %>% 
      mutate(gray_ML_prediction_1 = as.numeric(gray_ML_prediction_1),
             ntips_ML_gray = as.numeric(ntips_ML_gray) ,
             min_ML_gray= as.numeric(min_ML_gray),
             min_percent_ML_gray = as.numeric(min_percent_ML_gray)) %>% 
      mutate(filename = x) 
  ) 


all_ML_reconstructions_df_summarised <-  all_ML_reconstructions_df %>% 
  group_by(Feature_ID, `Proto-language`) %>% 
  summarise(gray_ML_prediction_0 = mean(gray_ML_prediction_0, na.rm = T),
            gray_ML_prediction_1 = mean(gray_ML_prediction_1, na.rm = T), 
            ntips_ML_gray = mean(ntips_ML_gray, na.rm = T), 
            min_ML_gray = mean(min_ML_gray), 
            min_percent_ML_gray = mean(min_percent_ML_gray), 
            .groups = "drop")

#summarising prediction into present or absent, or half-half
all_ML_reconstructions_df_summarised$gray_ML_prediction <- if_else(all_ML_reconstructions_df_summarised$gray_ML_prediction_0 > 0.6, "Absent", if_else(all_ML_reconstructions_df_summarised$gray_ML_prediction_1 > 0.6, "Present", "Half")) 

#add in HL findings
all_ML_reconstructions_df_summarised <- all_ML_reconstructions_df_summarised %>% 
  left_join(HL_findings_sheets,  by = c("Feature_ID", "Proto-language"))

#calculate the concordance with HL
all_ML_reconstructions_df_summarised$`ML result (Gray et al 2009-tree posteriors)` <- if_else(all_ML_reconstructions_df_summarised$gray_ML_prediction == "Present" & all_ML_reconstructions_df_summarised$Prediction == 1, "True Positive",  
                                                                                                            if_else(all_ML_reconstructions_df_summarised$gray_ML_prediction == "Absent" & all_ML_reconstructions_df_summarised$Prediction == 0, "True Negative",   
                                                                                                                    if_else(all_ML_reconstructions_df_summarised$gray_ML_prediction == "Absent" & all_ML_reconstructions_df_summarised$Prediction == 1, "False Negative",  
                                                                                                                            if_else(all_ML_reconstructions_df_summarised$gray_ML_prediction == "Present" & all_ML_reconstructions_df_summarised$Prediction == 0, "False Positive",
                                                                                                                                    
                                                                                                                                    ifelse(all_ML_reconstructions_df_summarised$gray_ML_prediction == "Half", "Half", NA)))))

##Marking which results can't be included because they don't have enough languages

#ML gray
all_ML_reconstructions_df_summarised$`ML result (Gray et al 2009-tree posteriors)`<- if_else(all_ML_reconstructions_df_summarised$ntips_ML_gray <  ntips_half_gray, "Not enough languages", all_ML_reconstructions_df_summarised$`ML result (Gray et al 2009-tree posteriors)`)

all_ML_reconstructions_df_summarised$`ML result (Gray et al 2009-tree posteriors)` <- ifelse(is.na(all_ML_reconstructions_df_summarised$Prediction), NA, all_ML_reconstructions_df_summarised$`ML result (Gray et al 2009-tree posteriors)`)

all_ML_reconstructions_df_summarised$gray_ML_prediction <- if_else(all_ML_reconstructions_df_summarised$ntips_ML_gray <  ntips_half_gray, "Not enough languages", all_ML_reconstructions_df_summarised$gray_ML_prediction)

all_ML_reconstructions_df_summarised %>% 
  write_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv")