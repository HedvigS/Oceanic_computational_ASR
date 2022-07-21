source("01_requirements.R")

#Make supplementary table of all GB features
GB_df_desc <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>% 
  filter(Binary_Multistate != "Multi") %>% 
  dplyr::select(`Feature\\_ID` = ID, Name)

cap <- "Table of Grambank fetures"
lbl <- "GB_features_table"
align <- c("r","p{3cm}", "p{12cm}") 

GB_df_desc %>% 
xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "results", "GB_features_supp_table.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, tabular.environment = "longtable", floating = F) 
