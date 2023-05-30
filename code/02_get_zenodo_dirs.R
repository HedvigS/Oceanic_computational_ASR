#This script downloads 

source("01_requirements.R")
source("fun_def_get_zenodo.R")

if(!dir.exists("../grambank_analysed/")){

get_zenodo_dir(url = "https://zenodo.org/record/7740822/files/grambank/grambank-analysed-v1.0.zip", exdir = "../grambank_analysed/")
}

if(!file.exists("../grambank_analysed/grambank/cldf/codes.csv")){
  
get_zenodo_dir(url = "https://zenodo.org/record/7740140/files/grambank/grambank-v1.0.zip", exdir = "../grambank_analysed/grambank/")
}

if(!file.exists("../grambank_analysed/glottolog-cldf/cldf/codes.csv")){
  
get_zenodo_dir(url ="https://zenodo.org/record/5772649/files/glottolog/glottolog-cldf-v4.5.zip", exdir = "../grambank_analysed/glottolog-cldf/")
}


if(!dir.exists("../dplace-data/")){
  
options(timeout=400)

get_zenodo_dir(url = "https://zenodo.org/record/5554395/files/D-PLACE/dplace-data-v2.2.1.zip", exdir = "../dplace-data")
}