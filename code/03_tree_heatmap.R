source("01_requirements.R")

#reading in glottolog language table (to be used for name labels)
glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Language_ID = Glottocode, Name) %>% 
  mutate(name_glottocode = paste0(Name, " [", Language_ID, "]"))

#get gray mcct tree
gray_tree_fn <-"output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt"
if(!file.exists(gray_tree_fn)){
  source("analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R")
}
gray_tree <- ape::read.tree(gray_tree_fn) 

#gray_tree <- keep.tip(phy = gray_tree, tip = sample(gray_tree$tip.label, 20))

gray_tree_tips <- gray_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(glottolog_df, by = "Language_ID")

gray_tree$tip.label <- gray_tree_tips$name_glottocode

#HL_findings sheet
HL_findings_sheet <- read_tsv(HL_findings_sheet_fn)


x <- colnames(read_tsv(GB_binary_fn, col_types = cols()))[3:203]

#reading in Grambank
GB_df_all <- read_tsv(GB_binary_fn, col_types = cols()) %>% 
  inner_join(gray_tree_tips, by = "Language_ID") %>% 
  column_to_rownames("name_glottocode") %>% 
  dplyr::select(-na_prop, -Language_ID, -Name)

png(filename = paste0(OUTPUTDIR_plots,"coverage_plots/", "tree_heatmap_gray_mcct.png"), width = 30, height = 27, units = "cm", res = 400)
phytools::phylo.heatmap(tree = gray_tree,X = GB_df_all, 
                        labels = F,
                        fsize = 0.5,
                        legend = F, 
                        offset = 0.0001, colors = c( "#ffffbf", "#8856a7"))
x <- dev.off()
