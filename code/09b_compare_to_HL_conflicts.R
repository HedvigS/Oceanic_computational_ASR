source("01_requirements.R")

GB_ID_desc <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>% 
  dplyr::select(Feature_ID = ID, Name)

#OUTPUTTING XTABLES
OUTPUT_DIR <- file.path( OUTPUTDIR_plots , "results")
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)}

HL_findings_sheet <- read_csv(HL_findings_sheet_conflicts_fn) %>% 
  rename(Prediction = Value) %>% 
  filter(!is.na(Prediction)) %>% 
  dplyr::select(Feature_ID, HL_prediction = Prediction, `Proto-language`, Source, Feature)

most_common_df <- read_tsv("output/HL_comparison/most_common_reconstructions.tsv") %>% 
  filter(!is.na(`result_most_common`)) %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, "most_common_prediction", "Proto-language", HL_prediction, Source)

#reading in the results from each method and tree and calculating the number of true negatives etc

parsimony_glottolog_df <- read_tsv("output/glottolog-tree/parsimony/all_reconstructions.tsv") %>% 
  right_join(HL_findings_sheet) %>%
  dplyr::select(Feature_ID, glottolog_parsimony_prediction, "Proto-language", HL_prediction, Source)

parsimony_gray_mcct_df <- read_tsv("output/gray_et_al_2009/parsimony/mcct/all_reconstructions.tsv") %>%
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, gray_parsimony_prediction, "Proto-language", HL_prediction, Source)

parsimon_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/parsimony/all_reconstructions_posteriors_aggregated.tsv")   %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, gray_parsimony_prediction_posteriors = gray_parsimony_prediction, "Proto-language", HL_prediction, Source)

ML_glottolog_df <- read_tsv("output/glottolog-tree/ML/all_reconstructions.tsv") %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, glottolog_ML_prediction, "Proto-language", HL_prediction, Source)

ML_gray_mcct <- read_tsv("output/gray_et_al_2009/ML/mcct/all_reconstructions.tsv") %>% 
  right_join(HL_findings_sheet) %>% 
  dplyr::select(Feature_ID, gray_ML_prediction, "Proto-language", HL_prediction, Source)

ML_gray_posteriors_df <- read_tsv("output/gray_et_al_2009/ML/all_reconstructions_posteriors_aggregated.tsv") %>% 
  right_join(HL_findings_sheet) %>%
  dplyr::select(Feature_ID, gray_ML_prediction_posteriors = gray_ML_prediction, "Proto-language", HL_prediction, Source)

#combining all
full_df <- parsimony_glottolog_df %>% 
  full_join(parsimony_gray_mcct_df) %>% 
  full_join(parsimon_gray_posteriors_df) %>% 
  full_join(ML_glottolog_df) %>% 
  full_join(ML_gray_mcct) %>% 
  full_join(ML_gray_posteriors_df) %>% 
  full_join(most_common_df) %>% 
  distinct()

#write xtable
cap <- "Table showing the results for the features where historical linguists disagree."
lbl <- "conflict_results_table"
align <- c("r", "p{4cm}","p{4cm}", "p{4cm}", "p{4cm}") 

xtable_conflicts <- full_df %>% 
  left_join(GB_ID_desc, by = "Feature_ID") %>% 
  distinct(Feature_ID, Name, `Proto-language`, 
           `Parsimony Glottolog (v4.4)` = glottolog_parsimony_prediction, 
           `Parsimony Gray et al (2009) - MCCT` = gray_parsimony_prediction,
           `Parsimony Gray et al (2009) - posteriors` = gray_parsimony_prediction_posteriors,
           `ML Glottolog (v4.4)`=glottolog_ML_prediction, 
           `ML Gray et al (2009) - MCCT`=gray_ML_prediction,
           `ML Gray et al (2009) - posteriors`=gray_ML_prediction_posteriors,
           `Most Common`=most_common_prediction) %>% 
  unite(Feature_ID, Name, `Proto-language`, col = "colname",sep = "$\\newline$") %>% 
    t() 

colnames(xtable_conflicts) <- paste0("$\\textbf{\\pb{ ", xtable_conflicts[1,], "}}$")
xtable_conflicts <- xtable_conflicts[-1,]

xtable_conflicts %>% 
  as.data.frame() %>%
  rownames_to_column("$\\textbf{\\parbox{1.8cm}{\\raggedright Method}}$") %>% 
  xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path(OUTPUT_DIR , "table_conflicts.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = T,
                       booktabs = TRUE) 
  
