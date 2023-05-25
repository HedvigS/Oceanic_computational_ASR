
#Function for getting ancestral states for 4 specific nodes out of castor parsimony objects
get_node_positions_parsimony <- function(GB_asr_object_parsimony, verbose = F){
  
  #  GB_asr_object_parsimony <- GB_ACR_all_parsimony$content[[187]]
  
  feature <- GB_asr_object_parsimony[[1]]
  ASR_object <- GB_asr_object_parsimony[[2]]
  tree <- GB_asr_object_parsimony[[4]]
  
  tip_label_df <- tree$tip.label %>% 
    as.data.frame() %>% 
    rename("Name" = ".") %>% 
    left_join(glottolog_df, by = "Name") 
  
  Polynesian_tips <- tip_label_df %>% 
    filter(str_detect(classification, "poly1242")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  polynesian_node <- getMRCA(tree, Polynesian_tips)
  
  oceanic_tips <- tip_label_df %>% 
    filter(str_detect(classification, "ocea1241")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  oceanic_node <- getMRCA(tree, oceanic_tips)
  
  central_oceanic_tips <- tip_label_df %>% 
    filter(str_detect(classification, "cent2060")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  central_oceanic_node <- getMRCA(tree, central_oceanic_tips)
  
  eastern_polynesian_tips <- tip_label_df %>% 
    filter(str_detect(classification, "east2449")) %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
  eastern_poly_node <- getMRCA(tree, eastern_polynesian_tips)
  
  df_proto_nodes <- tibble("Proto-language" = c("Proto-Oceanic", "Proto-Central Pacific", "Proto-Polynesian", "Proto-Eastern Polynesian") , Node = c(oceanic_node,  central_oceanic_node,  polynesian_node, eastern_poly_node))
  
  #  df_proto_nodes$Node <-   df_proto_nodes$Node - 1
  
  df_lik_anc <- as.data.frame(ASR_object$ancestral_likelihoods)
  df_lik_anc$Node <- seq(Ntip(tree) + 1, Ntip(tree) + nrow(df_lik_anc))  
  colnames(df_lik_anc) <- c("0", "1", "Node") 
  
  df <- df_proto_nodes %>% 
    left_join(df_lik_anc, by = "Node") %>% 
    mutate(Feature_ID = feature)
  
  if(verbose == T){
  cat("I'm done with finding the parsimony proto-language states for feature ", feature, ".\n", sep = "") }
  
  df
}


get_node_positions_ML <- function(GB_asr_object_ml, verbose = F ){
  
  #GB_asr_object_ml <- GB_ACR_all_ML$content[[12]]

if(GB_asr_object_ml[1] ==  "NA"){
  
  df <- tibble("Proto-language" = c("Proto-Oceanic", "Proto-Central Pacific", "Proto-Polynesian", "Proto-Eastern Polynesian"),
                   node = c(NA, NA, NA, NA),
                   `0`= c(NA, NA, NA, NA),
                   `1`= c(NA, NA, NA, NA),
                   Feature_ID = rep(GB_asr_object_ml[[2]]$Feature_ID, 4)
  )
  
  
} else{
  GB_asr_object_ml <- GB_asr_object_ml[[1]]
  
  feature <- GB_asr_object_ml$data %>% colnames() %>% .[2]
  tree <- GB_asr_object_ml$phy
  
  tip_label_df <- tree$tip.label %>% 
    as.data.frame() %>% 
    rename("Glottocode" = ".") 
  
  Polynesian_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "poly1242")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  polynesian_node <- getMRCA(tree, Polynesian_tips)
  
  oceanic_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "ocea1241")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  oceanic_node <- getMRCA(tree, oceanic_tips)
  
  central_oceanic_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "cent2060")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  central_oceanic_node <- getMRCA(tree, central_oceanic_tips)
  
  eastern_polynesian_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "east2449")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  eastern_poly_node <- getMRCA(tree, eastern_polynesian_tips)
  
  df_proto_nodes <- tibble("Proto-language" = c("Proto-Oceanic", "Proto-Central Pacific", "Proto-Polynesian", "Proto-Eastern Polynesian") , Node = c(oceanic_node,  central_oceanic_node,  polynesian_node, eastern_poly_node))
  
  df_lik_anc <- as.data.frame(GB_asr_object_ml$states)
  df_lik_anc$Node <- seq(Ntip(tree) + 1, Ntip(tree) + nrow(df_lik_anc))   # i.e. count from ntips + 1 …to .. ntips + number of nodes
  df <- df_proto_nodes %>% 
    left_join(df_lik_anc, by = "Node") %>% 
    mutate(Feature_ID = feature) %>% 
    rename(`0`= "(1,R1)" , `1` = "(2,R1)")
  
}
  if(verbose == T){
  cat("I'm done with finding the ML proto-language states for feature ", feature, ".\n", sep = "") }
  df
}


get_node_positions_SCM <- function(GB_asr_object_SCM, verbose = F){
  
  #GB_asr_object_SCM <- GB_ASR_RDS_SCM_gray$content[[1]]
  
  GB_asr_object_scm_SIMMAP_summary <- GB_asr_object_SCM[[1]]
  GB_asr_object_scm_results_df <- GB_asr_object_SCM[[2]]
  
  feature <- GB_asr_object_scm_results_df$Feature_ID
  tree <- GB_asr_object_SCM[[4]]
  
  tip_label_df <- tree$tip.label %>% 
    as.data.frame() %>% 
    rename("Glottocode" = ".") 
  
  Polynesian_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "poly1242")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  polynesian_node <- getMRCA(tree, Polynesian_tips)
  
  oceanic_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "ocea1241")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  oceanic_node <- getMRCA(tree, oceanic_tips)
  
  central_oceanic_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "cent2060")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  central_oceanic_node <- getMRCA(tree, central_oceanic_tips)
  
  eastern_polynesian_tips <- tip_label_df %>% 
    left_join(glottolog_df, by = "Glottocode") %>% 
    filter(str_detect(classification, "east2449")) %>% 
    dplyr::select(Glottocode) %>% 
    as.matrix() %>% 
    as.vector()
  
  eastern_poly_node <- getMRCA(tree, eastern_polynesian_tips)
  
  df_proto_nodes <- tibble("Proto-language" = c("Proto-Oceanic", "Proto-Central Pacific", "Proto-Polynesian", "Proto-Eastern Polynesian") , Node = c(oceanic_node,  central_oceanic_node,  polynesian_node, eastern_poly_node))
  
  df_lik_anc <-     GB_asr_object_SCM[[1]]$ace %>% 
    as.data.frame() %>% 
    rownames_to_column("Node") 
  
  df <- df_proto_nodes %>% 
    mutate(Node = as.character(Node)) %>% 
    left_join(df_lik_anc, by = "Node") %>% 
    mutate(Feature_ID = feature)

    if(verbose == T){
  cat("I'm done with finding the SCM proto-language states for feature ", feature, ".\n", sep = "") }
  df
}


