# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") }

pacman::p_load(
  tidyverse,
  reshape2,
  modEvA,
#  rsq,
#  cluster,
  viridis,
  rlang,
#  Amelia,
  readODS,
#  matrixcalc,
#  forcats,
#  knitr, 
  lazyeval,
#  gplots,
#  igraph,
#  geosphere,
 # diagram,
#  foreign, 
  MASS, 
  colorspace,
  RColorBrewer,
#  wesanderson,
  randomcoloR,
  ggridges,
  ggplot2,
  ggthemes,
#  tidytree ,
 # sandwich, 
 # msm,
  readxl,
  glue,
  broom, 
#  pscl,
  ggrepel,
jsonlite, #reading json files
  ggpubr,
 # cowplot,
  fuzzyjoin,
  infotheo,
  rlist,
  data.table,
  #making maps
  mapdata,
  maptools,
  maps,
  mapproj,
  ggmap,
#  qgraph,
  glue,
  stringi,
#  Rarity,
  ape, 
  castor,
  naniar, 
  fields,
  adephylo,
  phytools,
nloptr, 
GenSA,
#  diversitree,
  phylobase, 
  phangorn, 
#  treeman, 
  xtable,
  broom, 
#  sp, 
#  raster, 
  scales
)

#installing pacakge corHMM from specific website
if(str_detect(installed.packages()[,1] , "corHMM") %>% sum() == 0){
  install.packages("corHMM", repo = 'https://mac.R-project.org')
  library("corHMM")
  
}
library("corHMM")

#quieting down tidyverse
options(tidyverse.quiet = TRUE)

#setting up cut-off numbers for tree tips

ntips_half_gray <- 62 #half of gray et al tree tips that can be matched to GB
ntips_half_glottolog <- 117 #half of glottolog tree tips that can be matched to GB

##setting up folder structure 

#dirs for gray_et_al_tree
if (!dir.exists("output")) { dir.create("output") }
if (!dir.exists(file.path("output", "gray_et_al_2009"))) { dir.create(file.path("output", "gray_et_al_2009")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "ML"))) { dir.create(file.path("output", "gray_et_al_2009", "ML")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "ML"))) { dir.create(file.path("output", "gray_et_al_2009", "ML", "results_by_tree")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "parsimony"))) { dir.create(file.path("output", "gray_et_al_2009", "parsimony")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "parsimony"))) { dir.create(file.path("output", "gray_et_al_2009", "parsimony", "results_by_tree")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "SCM"))) { dir.create(file.path("output", "gray_et_al_2009", "SCM")) }
if (!dir.exists(file.path("output", "gray_et_al_2009", "SCM"))) { dir.create(file.path("output", "gray_et_al_2009", "SCM", "results_by_tree")) }

#dirs for glottolog-tree
if (!dir.exists(file.path("output", "glottolog_tree_binary"))) { dir.create(file.path("output", "glottolog_tree_binary")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "ML"))) { dir.create(file.path("output", "glottolog_tree_binary", "ML")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "parsimony"))) { dir.create(file.path("output", "glottolog_tree_binary", "parsimony")) }
if (!dir.exists(file.path("output", "glottolog_tree_binary", "SCM"))) { dir.create(file.path("output", "glottolog_tree_binary", "SCM")) }




