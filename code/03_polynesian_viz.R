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

feature_vec <- poly_tree_tip_value_df$GB409 %>% as.character()

#insert ? for missing
#poly_tree_tip_value_df$GB409 <- ifelse(is.na(poly_tree_tip_value_df$GB409), "?",  poly_tree_tip_value_df$GB409)

fmode <- as.factor(setNames(poly_tree_tip_value_df$GB409,poly_tree_tip_value_df$Name))

png(file = paste0(OUTPUTDIR_plots, "/tree_plots/poly_tree_example.png"), width = 8.27, height = 10.69, units = "in", res = 600)


dotTree(tree = ladderize(poly_tree,right = T),
        x = fmode,
        colors=setNames(colours_binary, c("0","1")),
        fsize=1, 
        cex.dot=1.3,
#        legend = F,
        border="transparent")

x <- dev.off()

#illustrating branch lengths

Glottolog_tree_full <- read.tree("output/processed_data/trees/glottolog_tree_newick_all_oceanic.txt") 

Glottolog_tree_pruned <- ape::keep.tip(Glottolog_tree_full, glottolog_df$Language_ID)

Glottolog_tree_pruned$tip.label <- Glottolog_tree_pruned$tip.label %>% 
  as.data.frame() %>% 
  rename(Language_ID = ".") %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  dplyr::select(Name) %>% 
  as.matrix() %>% 
  as.vector()

Glottolog_tree_pruned <- compute.brlen(Glottolog_tree_pruned, method = 1)

png(file = paste0(OUTPUTDIR_plots, "/tree_plots/poly_tree_example_brlen_glottolog_1.png"), width = 8.27, height = 10.69, units = "in", res = 600)

plot(ladderize(Glottolog_tree_pruned, right = T))

x <- dev.off()

png(file = paste0(OUTPUTDIR_plots, "/tree_plots/poly_tree_example_brlen_glottolog_grafen.png"), width = 8.27, height = 10.69, units = "in", res = 600)

plot(ladderize(compute.brlen(Glottolog_tree_pruned, method = "Grafen"), right = T))

x <- dev.off()

png(file = paste0(OUTPUTDIR_plots, "/tree_plots/poly_tree_example_brlen_gray.png"), width = 8.27, height = 10.69, units = "in", res = 600)

plot(ladderize(poly_tree, right = T))

x <- dev.off()

FN_multiphylo <- "output/processed_data/trees/gray_et_al_2009_posterios_pruned_multiPhylo.txt"
if(!file.exists(FN_multiphylo)){
  source("analysis_scripts_gray_all_posterior/03_process_gray_tree_posterios.R")}
trees <- read.tree(file = FN_multiphylo)

ultrametric_posteriors_n <- trees %>% is.ultrametric() %>% sum()
binary_posterios_n <- trees %>% is.binary() %>% sum()

cat(paste0("In the random sample of 100 trees from the Gray et al 2009-posterior ", ultrametric_posteriors_n , " were ultrametric and ", binary_posterios_n, " were binary.\n"))

#subsetting each tree in the multiphylo to only polynesian
for(tree_n in 1:length(trees)){
#  tree_n <- 14
  tree <- trees[tree_n]
  tree[[1]] <- ape::keep.tip(phy = tree[[1]], tip = poly_tree_tip_value_df$Language_ID)
  
  tree[[1]]$tip.label <- tree[[1]]$tip.label %>% 
    as.data.frame() %>% 
    rename(Language_ID = ".") %>% 
    left_join(glottolog_df, by = "Language_ID") %>% 
    dplyr::select(Name) %>% 
    as.matrix() %>% 
    as.vector()
  
   trees[tree_n] <- tree
}

png(filename = paste0(OUTPUTDIR_plots, "/tree_plots/poly_tree_example_brlen_gray_posterios.png"), width = 8.27, height = 10.69, units = "in", res = 600)
phangorn::densiTree(trees, 
                    col=colours_binary[1],  
                    tip.color = "black", 
                    scale.bar = F, 
                    alpha = 0.05,
                    type = "cladogram")
x <- dev.off()

