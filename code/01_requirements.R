# Please run this script first to make sure you have all the necessary packages 
# installed for running the rest of the scripts in this R project

source("fun_def_h_load.R")

#depdencies for castor
h_load(c("naturalsort", "RSpectra"))

h_load(dependencies = F, pkg = c(
  "tidyverse",
  "reshape2",
  "viridis",
#  rlang",
  "readODS",
"assertthat",
"beepr",
#  lazyeval",
  "gplots",
 # MASS", 
#  ggridges",
  "ggplot2",
 "grDevices",
#  ggthemes",
#  readxl",
# glue",
#  broom", 
#  ggrepel",
  "jsonlite", #reading json files
#  ggpubr",
#  psych",
#  fuzzyjoin",
#  infotheo",
#  rlist",
  "data.table",

    #making maps
  "mapdata",
  "maptools",
  "maps",
  "mapproj",
#  ggmap",
  "glue",
  "stringi",
  "ape", 
  "caper",
  "castor",
  "naniar", 
#  fields",
  "phytools",
  "adephylo",
  "phylobase",
  "nloptr", 
  "GenSA",
"wesanderson",
  "phangorn", #for acctran and dep of phytools
  "xtable",
  "broom", 
#  sf",
#  raster", 
  "scales", 
"janitor")
)

h_load("Rmpfr")

#installing pacakge corHMM from specific website
if(str_detect(installed.packages()[,1] , "corHMM") %>% sum() == 0){
  install.packages("corHMM", repo = 'https://mac.R-project.org')

}
library("corHMM")


#quieting down tidyverse
options(tidyverse.quiet = TRUE)
options(readr.show_col_types = FALSE)

#setting options for graphics (necessary for the cluster)
#options(bitmapType='cairo')

#setting up cut-off numbers for tree tips

ntips_half_gray <- 62 #half of gray et al tree tips that can be matched to GB
ntips_half_glottolog <- 131 #half of glottolog tree tips that can be matched to GB

##setting up folder structure for output
if (!dir.exists("output")) { dir.create("output") }

#dirs for gray_et_al_tree
fn <- "output/gray_et_al_2009"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/ML"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/ML/mcct"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/ML/results_by_tree"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/parsimony/"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/parsimony/mcct"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/parsimony/results_by_tree"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/scm/"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/scm/mcct"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/gray_et_al_2009/scm/results_by_tree"
if (!dir.exists(fn)) { dir.create(fn) }


#processed data dirs
fn <- "output/processed_data"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/processed_data/HL_findings/"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/HL_comparison/"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/processed_data/trees"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned"
if (!dir.exists(fn)) { dir.create(fn) }



#dirs for glottolog-tree
fn <- "output/glottolog-tree"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/ML"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/ML/mcct"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/ML/results_by_tree"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/parsimony/"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/parsimony/mcct"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/parsimony/results_by_tree"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/scm/"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/scm/mcct"
if (!dir.exists(fn)) { dir.create(fn) }

fn <- "output/glottolog-tree/scm/results_by_tree"
if (!dir.exists(fn)) { dir.create(fn) }



#dir for plots
#this project is meant to be working in tandem with tex code. To make that process easier, if there is a tex file one level up plots will be deposited there directly. However, if this code has been moved to somewhere else and no longer next to the tex folder, plots will just be straight in the output dir inside this dir.

if(dir.exists("../tex")){
 OUTPUTDIR_plots <- "../tex/illustrations/plots_from_R/"
 }else{
   OUTPUTDIR_plots<- "output"
}
if (!dir.exists(OUTPUTDIR_plots)) { dir.create(OUTPUTDIR_plots) }


fn <- file.path( OUTPUTDIR_plots , "coverage_plots")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots", "glottolog-tree")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots", "glottolog-tree", "parsimony")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots", "glottolog-tree", "ML")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots", "gray_et_al_2009")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots", "gray_et_al_2009", "parsimony")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "tree_plots", "gray_et_al_2009", "ML")
if (!dir.exists(fn)) { dir.create(fn) }


fn <- file.path( OUTPUTDIR_plots , "coverage_plots", "maps")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "coverage_plots", "tree")
if (!dir.exists(fn)) { dir.create(fn) }

fn <- file.path( OUTPUTDIR_plots , "coverage_plots", "tables")
if (!dir.exists(fn)) { dir.create(fn) }

#loading in tables that are used in multiple scripts

HL_findings_sheet_fn <- "output/processed_data/HL_findings/HL_findings_for_comparison.tsv"

HL_findings_sheet_conflicts_fn <- "data/HL_findings_conflicts.csv"

GB_df_desc_fn <- "output/GB_wide/parameters_binary.tsv"

GB_binary_fn <- "output/GB_wide/GB_wide_binarized.tsv"

glottolog_df_fn <- "output/processed_data/glottolog_language_table_wide_df.tsv"