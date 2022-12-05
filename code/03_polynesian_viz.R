source("01_requirements.R")

##TREE gray et al mcct
gray_tree_fn <-"output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt"
if(!file.exists(gray_tree_fn)){
  source("analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R")
}
gray_tree <- ape::read.tree(gray_tree_fn) 

GB_df_desc <- read_tsv(GB_df_desc_fn) %>% 
  filter(Binary_Multistate != "Multi")

#reading in GB
GB_df <- read_tsv(GB_binary_fn) %>% 
 dplyr::select(Language_ID, GB409) 
 
#reading in glottolog
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv") %>% 
  dplyr::select(Language_ID, classification, Name, level, med, Language_level_ID, Longitude, Latitude, Countries) %>% 
  filter(str_detect(classification, "poly1242")) %>% 
  inner_join(as.data.frame(gray_tree$tip.label) %>% #subsetting to lgs in the tree
               rename(Language_ID = "gray_tree$tip.label"), by = "Language_ID")

#wrangling tree
poly_tree <- ape::keep.tip(gray_tree,glottolog_df$Language_ID)

poly_tree_tip_value_df <- poly_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(GB_df, by = "Language_ID") %>% 
  left_join(glottolog_df, by = "Language_ID")

poly_tree$tip.label <- poly_tree_tip_value_df$Name

colours <- c("#ffffbf","#8856a7")
feature_vec <- poly_tree_tip_value_df$GB409 %>% as.character()

fmode<-as.factor(setNames(poly_tree_tip_value_df$GB409,poly_tree_tip_value_df$Name))

png(file = paste0(OUTPUTDIR_plots, "/tree_plots/poly_tree_example.png"), width = 8.27, height = 10.69, units = "in", res = 600)


dotTree(ladderize(poly_tree,right = F),x = fmode,colors=setNames(colours,
                                       c("0","1")),fsize=1, legend = F)



x <- dev.off()
