source("01_requirements.R")


#Make supplementary table of all GB features
GB_df_desc <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>% 
  filter(Binary_Multistate != "Multi") %>% 
  dplyr::select(Feature_ID = ID, Name)


cap <- "Table showing coverage of Oceanic languages in Grambank per island group."
lbl <- "GB_coverage_table_island_group_gray"
align <- c("r", "l","p{3cm}", "p{3cm}", "p{3cm}","p{3cm} ") 

xtable(caption = cap, label = lbl,
       digits = 0, 
       align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "coverage_plots", "tables","island_groups_table.tex"), sanitize.colnames.function = function(x){x},
                       include.rownames = FALSE, math.style.negative = F,
                       booktabs = TRUE, hline.after = c(-1, 0, nrow(island_groups_table_latex_formatting)-1, nrow(island_groups_table_latex_formatting))) 