# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

.libPaths(c("rlib/", .libPaths()))

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

if (!suppressPackageStartupMessages(require("pacman"))) { 
  install.packages("pacman", lib="./rlib/") 
  require("pacman")
  }

pacman::p_load(
  tidyverse,
  reshape2,
#  viridis,
#  rlang,
  readODS,
#  lazyeval,
#  gplots,
 # MASS, 
#  ggridges,
  ggplot2,
 grDevices,
#  ggthemes,
#  readxl,
# glue,
#  broom, 
#  ggrepel,
  jsonlite, #reading json files
#  ggpubr,
#  psych,
#  fuzzyjoin,
#  infotheo,
#  rlist,
  data.table,

    #making maps
  mapdata,
  maptools,
  maps,
  mapproj,
#  ggmap,
  glue,
  stringi,
  ape, 
  castor,
  naniar, 
#  fields,
  phytools,
  phylobase,
  nloptr, 
  GenSA,
  phangorn, #for acctran and dep of phytools
  xtable,
  broom, 
#  sf,
#  raster, 
  scales, 
janitor
)



#installing pacakge corHMM from specific website
if(str_detect(installed.packages()[,1] , "corHMM") %>% sum() == 0){
  install.packages("corHMM", repo = 'https://mac.R-project.org')
  library("corHMM")
  
}else{library("corHMM")}


#quieting down tidyverse
options(tidyverse.quiet = TRUE)
options(readr.show_col_types = FALSE)

#setting options for graphics (necessary for the cluster)
#options(bitmapType='cairo')

#setting up cut-off numbers for tree tips

ntips_half_gray <- 62 #half of gray et al tree tips that can be matched to GB
ntips_half_glottolog <- 131 #half of glottolog tree tips that can be matched to GB

##setting up folder structure 

#dirs for gray_et_al_tree
if (!dir.exists("output")) { dir.create("output") }
if (!dir.exists(file.path("output", "gray_et_al_2009"))) { dir.create(file.path("output", "gray_et_al_2009")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "ML"))) { dir.create(file.path("output", "gray_et_al_2009", "ML")) } 
if (!dir.exists(file.path("output", "gray_et_al_2009", "ML"))) { dir.create(file.path("output", "gray_et_al_2009", "ML", "mcct")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "ML"))) { dir.create(file.path("output", "gray_et_al_2009", "ML", "mcct", "tree_plots")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "ML"))) { dir.create(file.path("output", "gray_et_al_2009", "ML", "results_by_tree")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "parsimony"))) { dir.create(file.path("output", "gray_et_al_2009", "parsimony")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "parsimony"))) { dir.create(file.path("output", "gray_et_al_2009", "parsimony", "mcct")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "parsimony"))) { dir.create(file.path("output", "gray_et_al_2009", "parsimony", "mcct", "tree_plots")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "parsimony"))) { dir.create(file.path("output", "gray_et_al_2009", "parsimony", "results_by_tree")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "SCM"))) { dir.create(file.path("output", "gray_et_al_2009", "SCM")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "SCM", "results_by_tree"))) { dir.create(file.path("output", "gray_et_al_2009", "SCM", "results_by_tree")) }
if (!dir.exists(file.path("data", "trees", "gray_et_al_2009_posterior_trees_pruned"))) { dir.create(file.path("output", "gray_et_al_2009")) }


#dirs for glottolog-tree
if (!dir.exists(file.path("output", "glottolog_tree_binary"))) { dir.create(file.path("output", "glottolog_tree_binary")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "ML"))) { dir.create(file.path("output", "glottolog_tree_binary", "ML")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "parsimony"))) { dir.create(file.path("output", "glottolog_tree_binary", "parsimony")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "parsimony"))) { dir.create(file.path("output", "glottolog_tree_binary", "parsimony", "tree_plots")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "SCM"))) { dir.create(file.path("output", "glottolog_tree_binary", "SCM")) }



