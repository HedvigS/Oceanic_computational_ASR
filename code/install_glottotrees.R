# install packages
p_load("tidyverse","flextable","devtools", "ape")

devtools::install_github("rlesur/klippy")
devtools::install_github("erichround/phyloWeights", 
                         dependencies = T, 
                         INSTALL_opts = c("--no-multiarch"))

devtools::install_github("erichround/glottoTrees", 
                         dependencies = T, 
                         INSTALL_opts = c("--no-multiarch"))

library(glottoTrees)