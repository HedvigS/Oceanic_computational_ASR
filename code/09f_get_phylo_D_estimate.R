source("01_requirements.R")

output_dir <- "output/HL_comparison/phylo_d/"
if(!dir.exists(output_dir)){
  dir.create(output_dir)
  }


#tree fn vector to loop over
glottolog_tree_fn <- "output/processed_data/trees/glottolog_tree_newick_GB_pruned.txt"
gray_2009_mcct_tree_fn <- "output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt"
gray_posteriors_trees_fns <- list.files("output/processed_data/trees/gray_et_al_2009_posterior_trees_pruned/", pattern = "*.txt", full.names = T)

tree_fns <- c(glottolog_tree_fn, gray_2009_mcct_tree_fn, gray_posteriors_trees_fns)

#reading in GB tables
GB_df <- read_tsv(GB_binary_fn)

GB_df_desc <- read_tsv(GB_df_desc_fn, show_col_types = F) %>% 
  filter(!str_detect(Binary_Multistate, "Multi"))

#feature vector to loop over
features <- GB_df_desc$ID

#dfs to bind results to in each loop
full_df <- data.frame(Feature = as.character(), 
                      Destimate = as.numeric(), 
                      Pval1 = as.numeric(), 
                      Pval0 = as.numeric(), 
                      n = as.numeric(), 
                      tree = as.character(), 
                      zeroes = as.numeric(),
                      nPermut = as.numeric(),
                      Parameters_observed = as.numeric(),
                      Parameters_MeanRandom = as.numeric(),
                      Parameters_MeanBrownian = as.numeric(),
                      ones = as.numeric())

Permutations_full_df <- data.frame(
  Permutations_random = as.numeric(),
  Permutations_brownian = as.numeric(),
  Feature = as.character(),
  tree = as.character()
)

NodalVals_full_df <- data.frame(
  Node = as.factor(as.character()), 
  set = as.factor(as.character()),
  value = as.numeric(),
  Feature = as.character(),
  tree = as.character() 
)




for(f in 1:length(features)){
  
  #f <- 1
  feature <- features[f]
  fn_spec <- paste0(output_dir, "phylo_d_table_", feature)
  
  if(file.exists(fn_spec)){
    
    cat(paste0(fn_spec, " already exists! Moving on!\n"))
    
  }else{

  cat("\n***\nI'm on feature", feature, "which is", f, "out of", length(features),".\n***\n")
  for(t in tree_fns){
#    t <- tree_fns[1]
    tree <- read.tree(t)
  
    cat("I'm on feature", feature, "and tree", basename(t) ,".\n")
    
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

  output <- try(expr = {eval(substitute(phylo.d(data = ds, binvar = this_feature, permut = 20000), list(this_feature=as.name(feature))))})

    if (class(output) == "try-error") {
      spec_df <-   data.frame(Feature = output$binvar ,
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

        
    }
  
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
      rename(Node = Var1, set = Var2)
  
    
  NodalVals_full_df <- full_join(NodalVals_full_df, NodalVals_spec, by = c("Node", "set", "value", "Feature", "tree"))

  Permutations_full_df <- full_join(Permutations, Permutations_full_df, by = c("Permutations_random", "Permutations_brownian", "Feature", "tree"))
  
    full_df <- full_join(full_df, spec_df, by = c("Feature", "Destimate", "Pval1", "Pval0", "n", "tree", "zeroes", "ones", "nPermut", "Parameters_observed", "Parameters_MeanRandom", "Parameters_MeanBrownian"))
  }
  full_df %>% 
    write_tsv(file = paste0(fn_spec, ".tsv"), na = "")

    Permutations_full_df %>% 
    write_tsv(file = paste0(fn_spec,"_permutations", ".tsv"), na = "")

    NodalVals_full_df  %>% 
      write_tsv(file = paste0(fn_spec,"_NodalVals", ".tsv"), na = "")
    
    
      }
  
  
}

