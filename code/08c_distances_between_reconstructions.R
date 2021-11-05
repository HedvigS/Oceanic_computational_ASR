source("01_requirements.R")

#First we make a df with the basic counts for all oceanic languages per feature and what the distribution at the tips are. This will help us identify cases where all tips where of the same kind. These cases are currently excluded from the ML workflow since corHMM doesn't accept trees where the values are the same at all tips. The parsimony function however accepts such cases, so in order to make them comparable we need to put them back in to the results. We can use the basic summary table from the parsimony glottolog analysis for this since that per definition includes all lgs. Note that the total distribution over all lgs isn't the same as the distribution in the gray tree, which may be missing specific tips which have the divergent value.

HL_findings_sheet <- read_tsv("data/HL_findings/HL_findings_for_comparison.tsv") %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, Prediction, `Proto-language`) %>% 
  distinct(Feature_ID, Prediction, `Proto-language`)

most_common_values_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  dplyr::select(most_common_value = value, `Proto-language`, Feature_ID) %>% 
  distinct() %>% 
  filter(most_common_value != "Not enough languages")

#reading in the results from each method and tree and calculating the number of true negatives etc

parsimony_glottolog_df <- read_tsv("output/glottolog_tree_binary/parsimony/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, `Proto-language`, glottolog_parsimony_prediction)  %>% 
  distinct() %>% 
  filter(glottolog_parsimony_prediction != "Not enough languages")

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, `Proto-language`, gray_mcct_parsimony_prediction = gray_parsimony_prediction)%>% 
  distinct() %>% 
  filter(gray_mcct_parsimony_prediction != "Not enough languages")

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/results_by_tree/all_reconstructions_aggregate.tsv")   %>% 
  dplyr::select(Feature_ID, `Proto-language`, gray_posteriors_parsimony_prediction)%>% 
  distinct() %>% 
  filter(gray_posteriors_parsimony_prediction != "Not enough languages")

ML_glottolog_df <- read_tsv("output/glottolog_tree_binary/ML/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, `Proto-language`, glottolog_ML_prediction) %>% 
  full_join(most_common_values_df) %>% 
  mutate(glottolog_ML_prediction = ifelse(is.na(glottolog_ML_prediction),most_common_value , glottolog_ML_prediction)) %>% #bc the ML method fails when all the tips are the same state, such instances have an NA value in the ML results. We jsut replace those with the most common (the only) value to make it comparable to the parsimony results. Note that we're talking trees where every tip is the same, i.e. the state is the same for Proto-Polynesian, Proto-Oceanic etc. Not just the root!
  distinct() %>% 
  filter(glottolog_ML_prediction != "Not enough languages")

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  dplyr::select(Feature_ID, `Proto-language`,gray_mcct_ML_prediction = gray_ML_prediction) %>% 
  full_join(most_common_values_df) %>% 
  mutate(gray_mcct_ML_prediction = ifelse(is.na(gray_mcct_ML_prediction),most_common_value , gray_mcct_ML_prediction)) %>%  #bc the ML method fails when all the tips are the same state, such instances have an NA value in the ML results. We jsut replace those with the most common (the only) value to make it comparable to the parsimony results. Note that we're talking trees where every tip is the same, i.e. the state is the same for Proto-Polynesian, Proto-Oceanic etc. Not just the root!
  distinct() %>% 
filter(gray_mcct_ML_prediction != "Not enough languages")

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/results_by_tree/all_reconstructions_aggregate.tsv") %>% 
  filter(is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, `Proto-language`,gray_posteriors_ML_prediction) %>% 
  full_join(most_common_values_df) %>% 
  mutate(gray_posteriors_ML_prediction = ifelse(is.na(gray_posteriors_ML_prediction),most_common_value , gray_posteriors_ML_prediction)) %>%  #bc the ML method fails when all the tips are the same state, such instances have an NA value in the ML results. We jsut replace those with the most common (the only) value to make it comparable to the parsimony results. Note that we're talking trees where every tip is the same, i.e. the state is the same for Proto-Polynesian, Proto-Oceanic etc. Not just the root!
  distinct() %>% 
  filter(gray_posteriors_ML_prediction != "Not enough languages")
  
full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_values_df) %>%
  full_join(HL_findings_sheet) %>% 
  dplyr::select(glottolog_parsimony_prediction, gray_mcct_parsimony_prediction, gray_posteriors_parsimony_prediction, 
                glottolog_ML_prediction, gray_mcct_ML_prediction, gray_posteriors_ML_prediction, 
                most_common_value, HL_prediction = Prediction, everything()) 

df_for_daisy <- full_df %>% 
  dplyr::select(glottolog_parsimony_prediction, gray_mcct_parsimony_prediction, gray_posteriors_parsimony_prediction, glottolog_ML_prediction, gray_mcct_ML_prediction, gray_posteriors_ML_prediction, most_common_value, HL_prediction) 

df_for_daisy[df_for_daisy == "Present"] <- "1"
df_for_daisy[df_for_daisy == "Absent"] <- "0"
df_for_daisy[df_for_daisy == "Half"] <- "0.5"
df_for_daisy <- mutate_all(df_for_daisy, function(x) as.numeric(as.character(x)))

mdat <- df_for_daisy %>% 
  t() 

dist_matrix <- outer(1:nrow(mdat),1:nrow(mdat), FUN = Vectorize(function(i,j) { sum(abs((mdat[i,]-mdat[j,])), na.rm = T) }))

colnames(dist_matrix) <- colnames(df_for_daisy)
rownames(dist_matrix) <- colnames(df_for_daisy)

new <- df_for_daisy$gray_posteriors_ML_prediction - df_for_daisy$HL_prediction


dist_matrix %>% 
  heatmap.2(  key = F, symm = T,
   dendrogram = "none",
   revC = T,
   trace = "none", cellnote = round(dist_matrix, 2),
   margin=c(20,20), col=viridis(15, direction = -1))




