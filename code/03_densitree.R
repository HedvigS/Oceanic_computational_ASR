source("01_requirements.R")

if(!file.exists("data/trees/gray_et_al_2009_posterior_trees_pruned/gray_et_al_2009_posterior_tree_pruned_1.txt")){
  source("analysis_scripts_gray_all_posterior/03_process_gray_tree_posterios.R")}

fns <- list.files("data/trees/gray_et_al_2009_posterior_trees_pruned/", full.names = T)

h_load("phangorn")

trees <- read.tree(fns[1])

for(fn in fns[-1]){
#  fn <- fns[1]
tree <- read.tree(fn)
trees <-   c(trees, tree)
}


png("output/coverage_plots/tree/gray_et_al_2009_100_sample_densitree.png", width = 10.69, height = 10.69, units = "in", res = 600)
phangorn::densiTree(trees, col="chartreuse3",  tip.color = "white", scale.bar = F)
x <- dev.off()
