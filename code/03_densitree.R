source("01_requirements.R")
h_load("phangorn")

if(!file.exists(file.path("data", "trees", "posterios_pruned_multiPhylo.txt"))){
  source("analysis_scripts_gray_all_posterior/03_process_gray_tree_posterios.R")}

trees <- read.tree(file = file.path("data", "trees", "posterios_pruned_multiPhylo.txt"))

png("output/coverage_plots/tree/gray_et_al_2009_100_sample_densitree.png", width = 10.69, height = 10.69, units = "in", res = 600)
phangorn::densiTree(trees, col="chartreuse3",  tip.color = "white", scale.bar = F)
x <- dev.off()
