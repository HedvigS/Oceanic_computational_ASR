#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

source("01_requirements.R")

if(!dir.exists("../grambank-analysed/R_grambank/output")){
  dir.create("../grambank-analysed/R_grambank/output")}

setwd("../grambank-analysed/R_grambank/")

source("make_glottolog-cldf_table.R")

setwd("../../code/")

glottolog_language_table_wide_df <- read_tsv("../grambank-analysed/R_grambank/output/non_GB_datasets/glottolog-cldf_wide_df.tsv") %>% 
  mutate(Language_level_ID = ifelse(level == "language", Language_ID, Language_level_ID))

write_tsv(glottolog_language_table_wide_df, "output/processed_data/glottolog_language_table_wide_df.tsv")

#The languages-table from glottolog-cldf contains a paramter called "Language_ID" which is NOT the same as the parameter "Language_ID" in the values tables. This parameter is in fact the language leveled parent of a dialect. In order to avoid confusion, let's rename the parameter in the languages tables to the more transparent "Language_level_ID". This set-up first test if this is indeed a problem (i.e. if this is glottolog-cldf) and only does the renaming then.

cat("glottolog-cldf table created.\n")

#Making a list of language-leveled oceanic languages for glottolog tree for viz
glottolog_language_table_wide_df %>% 
  filter(str_detect(classification, "ocea1241")) %>% 
  filter(level =="language") %>% 
  write_tsv( "output/processed_data/glottolog_oceanic_languages_df.tsv")
