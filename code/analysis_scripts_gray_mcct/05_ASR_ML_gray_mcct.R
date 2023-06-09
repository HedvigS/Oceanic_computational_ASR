source("01_requirements.R")

glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, Language_level_ID, level, classification, Name)

#reading in GB
GB_df_desc <-  data.table::fread("../grambank-analysed/R_grambank/output/GB_wide/parameters_binary.tsv",
                                 encoding = 'UTF-8', 
                                 quote = "\"", 
                                 fill = T, 
                                 header = TRUE, 
                                 sep = "\t") %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

#reading in Grambank
GB_df_all <- read_tsv("../grambank-analysed/R_grambank/output/GB_wide/GB_wide_binarized.tsv", col_types = cols()) 

output_dir <- file.path("output", "gray_et_al_2009", "ML", "mcct")
if (!dir.exists(output_dir)) { dir.create(output_dir) }
if (!dir.exists(file.path(output_dir, "tree_plots"))) { dir.create(file.path(output_dir, "tree_plots")) } #dir for tree plot images

#reading in gray et all tree, already subsetted to only Oceanic and with tips renamed to glottocodes. If the tip was associated with a dialect which was individually coded in GB, the tip label is the glottocode for that dialect. If not, it has the language-level parent glottocode of that dialect. We'll be dropping tips with missing data feature-wise, i.e. for each feature not before.
gray_tree <- read.newick("output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt", quiet = T)

###ASR FUNCTION


fun_GB_ASR_ML <- function(feature) {

#feature <- "GB133"
cat("I've started ASR ML on ", feature, " with the Gray et al 2009-tree.\n", sep = "")
  
  filter_criteria <- paste0("!is.na(", feature, ")")
  
to_keep <- gray_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(GB_df_all, by = "Language_ID") %>% 
  filter(eval(parse(text = filter_criteria))) %>% #removing all tips that don't have data for the relevant feature
  dplyr::select(Language_ID, {{feature}})

gray_tree_pruned <- keep.tip(gray_tree, to_keep$Language_ID)  %>% ladderize(right = F)

feature_df <-  gray_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(GB_df_all, by = "Language_ID") %>% 
  dplyr::select(Language_ID, {{feature}}) 

#making a table for number of 0s and 1s, taking into account when there is only one of them. This will populate the columns for nTips_state_0 and nTips_state_1 later.

#counting the number of 0s and 1s
x <- feature_df[,2]  %>% table()

#checking if there are both 0s and 1s, or only just one of them
states <- length(x)
are_there_zeroes <- "0" %in% dimnames(x)[[1]]
are_there_ones <- "1" %in% dimnames(x)[[1]]

#making a count table in cases where there are no 0s
if(are_there_zeroes == F){
x <- tibble("0 " = 0,
            "1 " = x %>% as.matrix() %>% .[1,1])
}

#making a count table in cases where there are no 1s
if(are_there_ones == F){
  x <- tibble("0" = x %>% as.matrix() %>% .[1,1],
              "1" = 0)
}

#making a count table in cases where there are 0s and 1s
if(are_there_zeroes == T & are_there_ones == T){
  x <- tibble("0" = x %>% as.matrix() %>% .[1,1] %>% as.vector(),
              "1"= x %>% as.matrix() %>% .[2,1] %>% as.vector())
}

#make variables to use later
nTips_state_0 = x$`0`[1]
nTips_state_1 = x$`1`[1]

if(states == 1| nTips(gray_tree_pruned) < ntips_half_gray) {
  message("All tips for feature ", feature, " are of the same state or there are too few tips. We're skipping it, we won't do any ASR or rate estimation for this feature.\n")
#  beepr::beep(9)
  
  results_df <- data.frame(
    Feature_ID = feature,
    LogLikelihood = NA,
    AICc = NA,
    pRoot0 = NA,
    pRoot1 = NA,
    q01 = NA,
    q10 = NA,
      nTips = nTips(gray_tree_pruned),
    nTips_state_0 =  nTips_state_0,
    nTips_state_1 = nTips_state_1)
    
  output <- list("NA", results_df)
  output
} else{


# If I decide to switch back and have unknown tips in, replace ? or missing tips with "0&1" or leave as NA and don't prune

corHMM_result_direct <- corHMM::corHMM(
      phy = gray_tree_pruned , 
      data = feature_df, 
      model="ARD",
      rate.cat = 1,
      lewis.asc.bias = TRUE,
      node.states = "marginal",  # joint, marginal, scaled
      root.p = "yang" 
    )

results_df <- data.frame(
  Feature_ID = feature,
  LogLikelihood = corHMM_result_direct$loglik,
  AICc = corHMM_result_direct$AICc,
  pRoot0 = corHMM_result_direct$states[1, 1],
  pRoot1 = corHMM_result_direct$states[1, 2],
  q01 = corHMM_result_direct$solution[1,][2],
  q10 = corHMM_result_direct$solution[2,][1],
  nTips = phylobase::nTips(gray_tree_pruned), 
  nTips_state_0 =  nTips_state_0,
  nTips_state_1 = nTips_state_1
)

gray_tree_pruned_tip.labels_df <- gray_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df, by = "Glottocode") 

gray_tree_pruned$tip.label <- gray_tree_pruned_tip.labels_df$Name

gray_tree_pruned$root.edge <- 0.3

png(filename = paste0(OUTPUTDIR_plots, "/tree_plots/gray_et_al_2009/ML/", "ML_gray_mcct_-", feature, ".png"), width = 15, height = 22, units = "cm", res = 400)

plotRECON_tweaked(gray_tree_pruned, corHMM_result_direct$states, font=1, cex = 0.4,
                  use.edge.length = TRUE,
                  piecolors=colours_binary,
                  show.legend = F,
                  no.margin = T,
                  root.edge = T,
                  tip_states = feature_df[,2],
                  text.x = 132, text.pos = 4, text.cex = 1,
                  title = paste0("Gray et al (2009)-mcct, ML: ", feature)
)
x <- dev.off()

cat("Done with ASR ML on ", feature, ".\n", sep = "")

#beepr::beep(2)
output <- list(corHMM_result_direct, results_df)
 output
}
}


GB_ASR_ML_all <- tibble(Feature_ID = GB_df_desc$ID,
                                   content = purrr::map(GB_df_desc$ID,
                                                        fun_GB_ASR_ML ))

#beepr::beep(3)

saveRDS(GB_ASR_ML_all, file = file.path(output_dir, "GB_ML_gray_tree.rds"))

##unraveling the output into a summary table

GB_ASR_ML_all_split  <- GB_ASR_ML_all %>% 
  unnest(content) %>% 
  group_by(Feature_ID) %>% 
  mutate(col=seq_along(Feature_ID)) %>%
  spread(key=col, value=content) %>% 
  rename(corHMM_result_direct = "1", results_df = "2") %>% 
  ungroup()


#making empty df to rbind to

results <- data.frame(
  Variable = NULL,
  LogLikelihood = NULL,
  AICc = NULL,
  pRoot0 = NULL,
  pRoot1 = NULL,
  q01 = NULL,
  q10 = NULL,
  nTips = NULL,
  nTips_state_0 =  NULL,
  nTips_state_1 =  NULL
)


for(row in GB_ASR_ML_all_split$results_df){
  print(row)
results <- rbind(results, row)
  }

write_csv( results, file = file.path(output_dir,"results.csv"))