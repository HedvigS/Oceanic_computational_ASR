source("01_requirements.R")

args = commandArgs(trailingOnly = TRUE)

if(length(args) != 0){
  start = args[1]
  end <- args[2]
  range <- start:end
} else { #if you're running this script chunkwise in Rstudio or similar instead of via command line, you'll read in the parameters this way:
  start <- 1
  end <- 201
  range <- start:end
}

output_dir <- "output/HL_comparison/phylo_d/"
if(!dir.exists(output_dir)){
  dir.create(output_dir)
  }

#tree fn vector to loop over
glottolog_tree_fn <- "output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt"
gray_2009_mcct_tree_fn <- "output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt"
gray_posteriors_trees_fns <- list.files("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", pattern = "*.txt", full.names = T)

tree_fns <- c(glottolog_tree_fn, gray_2009_mcct_tree_fn, gray_posteriors_trees_fns)[-c(1:3)]

#reading in GB tables
GB_df <- read_tsv(GB_binary_fn)

GB_df_desc <- read_tsv(GB_df_desc_fn, show_col_types = F) %>% 
  filter(!str_detect(Binary_Multistate, "Multi"))

#feature vector to loop over
features <- GB_df_desc$ID[range]

for(f in 1:length(features)){
  
  #f <- 17
  feature <- features[f]
  fn_spec <- paste0(output_dir, "phylo_d_table_", feature)
  
  cat("\n***\nI'm on feature", feature, "which is", f, "out of", length(features),". ", as.character(Sys.time()), ".\n***\n")
  for(t in tree_fns){
#    t <- tree_fns[2]
    tree <- read.tree(t)

    cat("I'm on feature", feature, "and tree", basename(t) , as.character(Sys.time()),".\n")
    
    fn_spec_main <- paste0(fn_spec, "_", basename(t), "_main.qs")
  
    if(file.exists(fn_spec_main)){

    cat(paste0("File exists, moving to next!\n"))
      
    }else{
    df_for_caper <- tree$tip.label %>%
      as.data.frame() %>%
      rename(Language_ID = ".") %>%
      left_join(GB_df, by = "Language_ID") %>% 
      dplyr::select(Language_ID, all_of(feature))
    
    ds <- comparative.data(tree, df_for_caper, names.col=Language_ID)
    
    value_table <- df_for_caper[,2] %>% table() %>% t()
    
    if(ncol(value_table) == 2){
      zeroes = value_table[1,"0"]
      ones = value_table[1,"1"]
      }

    if(all(ncol(value_table) == 1 & colnames(value_table) == "0")){
      zeroes = value_table[1,"0"]
      ones = 0
    }
  if(all(ncol(value_table) == 1 & colnames(value_table) == "1")){
      zeroes = 0
      ones = value_table[1,"1"]
    }

if(ncol(value_table) == 2){
      output <- try(expr = {eval(substitute(phylo.d(data = ds, binvar = this_feature, permut = 20000), list(this_feature=as.name(feature))))})
}
    
#  cat("done with phylo.d function.", as.character(Sys.time()), ".\n")
  
    if (class(output) == "try-error"|ncol(value_table) != 2) {
      spec_df <-   data.frame(Feature = feature ,
                              Destimate = NA, 
                              Pval1 = NA, 
                              Pval0 = NA, 
                              Parameters_observed = NA,
                              Parameters_MeanRandom = NA,
                              Parameters_MeanBrownian = NA,
                              
                              nPermut = NA,
                              n = ds$data %>% nrow(), 
                              tree = basename(t), 
                              
                              zeroes = zeroes, 
                              ones = ones)
      
    Permutations <- data.frame(
        Permutations_random = NA,
        Permutations_brownian = NA,
        Feature = feature,
        tree = basename(t)
      )
      
    NodalVals_spec<- data.frame(
        Node = NA, 
        set = NA,
        value = NA,
        Feature = feature,
        tree = basename(t)
      )

    }else{
    
    spec_df <-   data.frame(Feature =output$binvar, 
                            Destimate = output$DEstimate[[1]], 
                            Pval1 = output$Pval1, 
                            Pval0 = output$Pval0, 
                            n = ds$data %>% nrow(), 
                            tree = basename(t), 
                            zeroes =  zeroes, 
                            ones = ones,
                            Parameters_observed = output$Parameters$Observed,
                            Parameters_MeanRandom = output$Parameters$MeanRandom,
                            Parameters_MeanBrownian =     output$Parameters$MeanBrownian,
                            nPermut = output$nPermut
    )
    

    #Permutations df
        Permutations <- cbind(output$Permutations$random, output$Permutations$brownian) %>% as.data.frame()
        Permutations[,3] <- rep(output$binvar, nrow(Permutations))
        Permutations[,4] <- rep(basename(t), nrow(Permutations))
        colnames(Permutations) <- c("Permutations_random", "Permutations_brownian", "Feature", "tree")

  #NodalVals df              
    NodalVals_obs <-   output$NodalVals$observed 
    colnames(NodalVals_obs) <- rep("observed", ncol(NodalVals_obs))

    NodalVals_random <-   output$NodalVals$random 
    colnames(NodalVals_random) <- rep("random", ncol(NodalVals_random))

    NodalVals_brownian <-   output$NodalVals$brownian 
    colnames(NodalVals_brownian) <- rep("brownian", ncol(NodalVals_brownian))

    NodalVals_spec <- NodalVals_obs %>% 
      cbind(NodalVals_random) %>% 
      cbind(NodalVals_brownian) %>% 
      reshape2::melt() %>% 
      mutate(Feature = output$binvar,
             tree = basename(t)) %>% 
      rename(Node = Var1, set = Var2) %>% 
      mutate(Node = as.factor(Node))

    }
  spec_df  %>% 
  qs::qsave(file = fn_spec_main, preset = "archive")
 
   Permutations %>% 
   qs::qsave(file = paste0(fn_spec, "_", basename(t), "_permutations.qs"), preset = "archive")
   
   NodalVals_spec %>% 
   qs::qsave(file = paste0(fn_spec, "_", basename(t), "_Nodalvals.qs"), preset = "archive")
   
    }
  }
}