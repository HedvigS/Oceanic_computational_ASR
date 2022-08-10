#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

source("01_requirements.R")

if(!dir.exists("../grambank-analysed/R_grambank/output")){
  dir.create("../grambank-analysed/R_grambank/output")}

setwd("../grambank-analysed/R_grambank/")

source("make_wide.R")
source("make_wide_binarized.R")

setwd("../../code/")

#move files to subdir of output to make easier for collab
fn <- file.path("output/GB_wide/")
if (!dir.exists(fn)) { dir.create(fn) }

read_tsv("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv") %>%
  write_tsv("output/GB_wide/parameters_binary.tsv")
  
read_tsv("../grambank-analysed/R_grambank/output/GB_wide/GB_wide_binarized.tsv") %>% 
  write_tsv("output/GB_wide/GB_wide_binarized.tsv")