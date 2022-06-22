#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

source("01_requirements.R")

if(!dir.exists("../grambank-analysed/R_grambank/output")){
  dir.create("../grambank-analysed/R_grambank/output")}

setwd("../grambank-analysed/R_grambank/")

source("make_wide.R")
source("make_wide_binarized.R")

setwd("../../code/")
