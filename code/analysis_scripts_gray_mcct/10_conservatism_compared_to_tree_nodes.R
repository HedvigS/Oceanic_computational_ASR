source("01_requirements.R")


rm(list=setdiff(ls(), "start_time")) #cleaning environment

subregions <- read_tsv("../data/Remote_Oceania_Political_complex_and_more/oceania_subregions.tsv") %>% 
  rename(glottocode = Glottocode)


range01 <- function(x){(x-min(x))/(max(x)-min(x))}

taxa <- read_csv("../../dplace-data/phylogenies/gray_et_al2009/taxa.csv")


parsimony_gray <- read_tsv("output/ASR/conservatism_parsimony_gray.tsv") %>% 
  mutate(`Mean parsimony cost` = range01(`Mean parsimony cost`)) %>% 
  rename(parsimony_cost_gray = `Mean parsimony cost`) %>% 
  rename(taxon = glottocode) %>% 
  left_join(taxa) %>% 
  dplyr::select(glottocode, parsimony_cost_gray, nNodes_mean_MP_gray = nNodes_mean)

parsimony_glottolog <- read_tsv("output/ASR/conservatism_parsimony_glottolog.tsv") %>% 
  mutate(`Mean parsimony cost` = range01(`Mean parsimony cost`)) %>% 
  rename(parsimony_cost_glottolog = `Mean parsimony cost`) %>% 
  dplyr::select(parsimony_cost_glottolog, glottocode, nNodes_mean_MP_glottolog = nNodes_mean)


ML_gray <- read_tsv("output/ASR/conservatism_ML_gray.tsv") %>% 
  mutate(mean_dist = range01(mean_dist)) %>% 
  rename(ML_dist_gray = mean_dist) %>% 
  dplyr::select(ML_dist_gray, glottocode, nNodes_mean_ML_gray = nNodes_mean)

ML_glottolog <- read_tsv("output/ASR/conservatism_ML_glottolog.tsv") %>% 
  mutate(mean_dist = range01(mean_dist)) %>% 
  rename(ML_dist_glottolog = mean_dist) %>% 
  dplyr::select(ML_dist_glottolog , glottocode, nNodes_mean_ML_glottolog = nNodes_mean)

nodes_between_df <-  full_join(ML_glottolog, ML_gray) %>% 
  full_join(parsimony_glottolog) %>% 
  full_join(parsimony_gray) %>% 
  left_join(subregions) %>% 
  group_by(Abberancy_group) %>% 
  summarise(mean_nNodes_mean_ML_glottolog = mean(nNodes_mean_ML_glottolog, na.rm = T),
            mean_nNodes_mean_ML_gray  = mean(nNodes_mean_ML_gray, na.rm = T ),
            mean_nNodes_mean_MP_glottolog = mean(nNodes_mean_MP_glottolog, na.rm = T),
            mean_nNodes_mean_MP_gray  = mean(nNodes_mean_MP_gray, na.rm = T )) %>% 
  ungroup() %>% 
  mutate(mean_all_nodes = ((mean_nNodes_mean_ML_glottolog+ mean_nNodes_mean_ML_gray+ mean_nNodes_mean_MP_glottolog+mean_nNodes_mean_MP_gray)/4 ))

nodes_between_df %>% 
  dplyr::select(`Island group` = Abberancy_group,`Mean number of nodes between tips and root`= mean_all_nodes ) %>% 
  arrange(`Mean number of nodes between tips and root`) %>% xtable() %>% 
  xtable::print.xtable(include.rownames = F)


nodes_between_df %>% 
  dplyr::select(`Island group` = Abberancy_group,
                `All`= mean_all_nodes, 
                `MP Glottolog` = mean_nNodes_mean_MP_glottolog,
                `MP Gray et al 2009` = mean_nNodes_mean_MP_gray,  
                `ML Glottolog` = mean_nNodes_mean_ML_glottolog,
                `ML Gray et al 2009` = mean_nNodes_mean_ML_gray) %>% 
  arrange(All) %>% xtable() %>% 
  xtable::print.xtable(include.rownames = F)



nNodes_mean_ML_gray 
######

ML_glottolog_plot <- ML_glottolog %>% 
  ggplot(aes(y= ML_dist_glottolog, x = nNodes_mean_ML_glottolog)) +
  geom_point(color = "turquoise3", shape = 21) +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm') +
  theme_classic() +
  ylab(label = "ML branch lengths (Glottolog)") +
  xlab(label = "Number of nodes from tip to root") +
  xlim(c(0, 25))

ML_gray_plot <- ML_gray %>% 
  ggplot(aes(y= ML_dist_gray, x = nNodes_mean_ML_gray)) +
  geom_point(color = "darkgoldenrod3", shape = 21) +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm') +
  theme_classic()  +
  ylab(label = "ML branch lengths (Gray et al 2009)") +
  xlab(label = "Number of nodes from tip to root")+
  xlim(c(0, 20))


parsimony_glottolog_plot <- parsimony_glottolog %>% 
  ggplot(aes(y= parsimony_cost_glottolog, x =  nNodes_mean_MP_glottolog)) +
  geom_point(color = "hotpink1", shape = 21) +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm') +
  theme_classic() +
  ylab(label = "Parsimony cost (Glottolog)") +
  xlab(label = "Number of nodes from tip to root") +
  xlim(c(0, 25))

parsimony_gray_plot <- parsimony_gray %>% 
  ggplot(aes(y= parsimony_cost_gray, x =  nNodes_mean_MP_gray)) +
  geom_point(color = "darkmagenta", shape = 21) +
  ggpubr::stat_cor(method = "pearson", p.digits = 2, geom = "label", color = "blue",
                   label.y.npc="top", label.x.npc = "left", alpha = 0.8) +
  geom_smooth(method='lm') +
  theme_classic() +
  ylab(label = "Parsimony cost (Gray et al 2009)") +
  xlab(label = "Number of nodes from tip to root") +
  xlim(c(0, 20))



library(ggpubr)

ggpubr::ggarrange(parsimony_glottolog_plot, parsimony_gray_plot, ML_glottolog_plot , ML_gray_plot)

ggsave("output/ASR/other_plots/br_len_compare_to_nodes.png", height = 8, width = 8)


