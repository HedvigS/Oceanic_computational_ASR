# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") }
cat("checked pacman")

pacman::p_load(
  tidyverse,
  missForest,
  reshape2,
  modEvA,
  rsq,
  cluster,
  viridis,
  rlang,
  Amelia,
  readODS,
  devtools,
  matrixcalc,
  forcats,
  knitr, 
  lazyeval,
  gplots,
  igraph,
  geosphere,
  diagram,
  foreign, 
  MASS, 
  colorspace,
  RColorBrewer,
  wesanderson,
  randomcoloR,
  ggridges,
  ggplot2,
  ggthemes,
  tidytree ,
  sandwich, 
  msm,
  readxl,
  glue,
  broom, 
  pscl,
  ggrepel,
  ggpubr,
  cowplot,
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
  qgraph,
  glue,
  stringi,
  Rarity,
  ape, 
  castor,
  naniar, 
  fields,
  adephylo,
  phytools,
  diversitree,
  phylobase, 
  phangorn, 
  treeman, 
  devtools,
  xtable,
  broom, 
  sp, 
  raster, 
  scales
)
cat("checked rest of packages")

if(str_detect(installed.packages()[,1] , "corHMM") %>% sum() == 0){
  install.packages("corHMM", repo = 'https://mac.R-project.org')
  library("corHMM")
  
}
library("corHMM")

cat("checked corHMM")


#quieting down tidyverse
options(tidyverse.quiet = TRUE)
