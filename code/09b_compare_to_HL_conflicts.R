source("01_requirements.R")

#OUTPUTTING XTABLES
OUTPUT_DIR <- file.path( OUTPUTDIR_plots , "results")
if(!dir.exists(OUTPUT_DIR)){
  dir.create(OUTPUT_DIR)}

#this script makes to tables that summarises information regarding the states that HL disagrees on.

#reading in table with full features names
GB_ID_desc <- read_tsv(GB_df_desc_fn) %>% 
  dplyr::select(Feature_ID = ID, Name)

#table with conflicts in HL first, including precise sources
HL_findings_sheet_conflicts <- read_csv(HL_findings_sheet_conflicts_fn) %>% 
  rename(Prediction = Value) %>% 
  mutate(Prediction = str_replace_all(Prediction, "0", "Absent")) %>% 
  mutate(Prediction = str_replace_all(Prediction, "1", "Present")) %>% 
  filter(!is.na(Prediction))  %>%
  left_join(GB_ID_desc, by = "Feature_ID") %>% 
  dplyr::select("Proto-language",`Feature ID` = Feature_ID,  Prediction,Source = "Source (Latex)")  %>% 
  mutate(Source = str_replace_all(Source, "clark1976aspects", "clark1973aspects")) %>% 
  mutate(Source = str_replace_all(Source, "\\{ball", "\\citet{ball"))
  

colnames(HL_findings_sheet_conflicts) <- paste0("$\\textbf{\\pb{ ", colnames(HL_findings_sheet_conflicts), "}}$")

#write xtable
cap <- "Table showing the features where historical linguists disagree"
lbl <- "conflict_table"
align <- c("p{6cm}", "p{4.5cm}","p{2cm}", "p{2cm}", "p{5cm}") 

HL_findings_sheet_conflicts %>% 
  xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path(OUTPUT_DIR , "table_conflicts_HL.tex"), 
                       sanitize.text.function =  function(x){x},
                       sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = T,
                       booktabs = TRUE) 

#table with the results of the predictions from comp methods

full_df <- read_tsv("output/all_reconstructions_all_methods_long.tsv") %>%  
  filter(variable == "prediction") %>% 
  filter(!is.na(value)) %>% 
  filter(!is.na(conflict)) %>% 
  dplyr::select(-variable, -conflict) %>% 
  unite(method, tree_type, col = "method", sep = " ") %>% 
  mutate("method" = str_replace_all(`method`, "parsimony", "MP")) %>% 
  mutate("method" = str_replace_all(`method`, "gray_mcct", "Gray et al (2009) - MCCT")) %>% 
  mutate("method" = str_replace_all(`method`, "gray_posteriors", "Gray et al (2009) - posteriors")) %>% 
  mutate("method" = str_replace_all(`method`, "glottolog", "Glottolog")) %>% 
  mutate("method" = ifelse(str_detect(method, "common"), "Most common", method)) %>% 
   unite(Feature_ID, "Proto-language", col = "Feature", sep = " ") %>% 
    reshape2::dcast(Feature ~ method, value.var = "value") %>% 
  dplyr::select(Feature, "MP Glottolog",
                "MP Gray et al (2009) - MCCT",       
                "MP Gray et al (2009) - posteriors" , 
                "ML Glottolog", 
                "ML Gray et al (2009) - MCCT"   ,
                "ML Gray et al (2009) - posteriors" ,
                "Most common") 

full_df <- full_df %>% t()

colnames(full_df) <- paste0("$\\textbf{\\pb{ ", full_df[1,], "}}$")
full_df <- full_df[-1,] %>% 
  as.data.frame() %>% 
  rownames_to_column("$\\textbf{\\pb{Method}}$")

#write xtable
cap <- "Table showing the computational results for the features where historical linguists disagree"
lbl <- "conflict_results_table"
align <- c("p{6cm}", "p{5cm}","p{3cm}", "p{3cm}", "p{3cm}") 

full_df %>% 
  xtable(caption = cap, label = lbl,
         digits = 0, 
         align = align) %>% 
  xtable::print.xtable(file = file.path(OUTPUT_DIR , "table_conflicts_results.tex"), 
                       sanitize.text.function = function(x){x},
                       sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = T,
                       booktabs = TRUE) 
  
