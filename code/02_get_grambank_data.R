#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

source("01_requirements.R")

#if the git submodules aren't initialized, get the data from Zenodo-urls
source("02_get_zenodo_dirs.R")

if(!dir.exists("../grambank-analysed/R_grambank/output/")){
  dir.create("../grambank-analysed/R_grambank/output/")}

setwd("../grambank-analysed/R_grambank/")

source("make_wide.R")
source("make_wide_binarized.R")

setwd("../../code/")

## move files to subdir of output to make easier for collab
# creating output dir
fn <- file.path("output/GB_wide/")
if (!dir.exists(fn)) { dir.create(fn) }

# moving the feature description table
read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>%
  write_tsv("output/GB_wide/parameters_binary.tsv")

#making a list of oceanic lgs
oceanic_lgs <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv") %>% 
  filter(str_detect(classification, "ocea1241")|Language_ID == "ocea1241") %>% 
  dplyr::select(Language_ID)

#taking the GB widening and binarised and subsetting to oceanic and moving.
read_tsv("../grambank-analysed/R_grambank/output/GB_wide/GB_wide_binarized.tsv") %>% 
  inner_join(oceanic_lgs, by = "Language_ID" ) %>%
  write_tsv("output/GB_wide/GB_wide_binarized.tsv")