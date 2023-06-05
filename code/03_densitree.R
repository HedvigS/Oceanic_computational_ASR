source("01_requirements.R")

FN_multiphylo <- "output/processed_data/trees/gray_et_al_2009_posterios_pruned_multiPhylo.txt"
if(!file.exists(FN_multiphylo)){
  source("analysis_scripts_gray_all_posterior/03_process_gray_tree_posterios.R")}
trees <- read.tree(file = FN_multiphylo)

png(filename = paste0(OUTPUTDIR_plots, "coverage_plots/tree/gray_et_al_2009_100_sample_densitree.png"), width = 10.69, height = 10.69, units = "in", res = 600)
phangorn::densiTree(trees, 
                    col=colours_binary[1],  
                    alpha = 0.05, 
                    tip.color = "white", 
                    scale.bar = F)
x <- dev.off()