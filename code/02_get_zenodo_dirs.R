#This script downloads 

source("01_requirements.R")
source("fun_def_get_zenodo.R")


if(!file.exists("../grambank-analysed/R_grambank/coverage_bar_plots.R")){

#checking out specifically version 1.0
get_zenodo_dir(url = "https://zenodo.org/records/7740822/files/grambank/grambank-analysed-v1.0.zip", exdir = "../grambank-analysed/")
}


if(!file.exists("../grambank-analysed/grambank/cldf/codes.csv")){
  
#checking out specifically v1.0, which is commit 9e0f341
get_zenodo_dir(url = "https://zenodo.org/records/7740140/files/grambank/grambank-v1.0.zip", exdir = "../grambank-analysed/grambank/")
}

#checking out specifically version v4.5, which is commit df1b79f
if(!file.exists("../grambank-analysed/glottolog-cldf/cldf/codes.csv")){
  
get_zenodo_dir(url ="https://zenodo.org/records/5772649/files/glottolog/glottolog-cldf-v4.5.zip", exdir = "../grambank-analysed/glottolog-cldf/")
}


if(!dir.exists("../dplace-data/")){
  
options(timeout=400) #dplace-data is so large,we need  to set the timeout to larger than default (default = 60).

#getting specifically v2.2.1
get_zenodo_dir(url = "https://zenodo.org/records/5554395/files/D-PLACE/dplace-data-v2.2.1.zip", exdir = "../dplace-data")
}