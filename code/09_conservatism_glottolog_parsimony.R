source("01_requirements.R")

glottolog <- read_tsv("data/Glottolog_lookup_table_Heti_edition.tsv") %>%
  dplyr::select(glottocode, Name_stripped_no_spaces, Family_name, Path)

#reading in glottolog tree
Glottolog_tree_full <- read.tree("data/trees/glottolog4.1_grambank_pruned.newick")

#renaming tips to names instead of glottocodes
Glottolog_tree_full_tips_df <- Glottolog_tree_full$tip.label %>% 
  as.data.frame() %>% 
  rename(glottocode = ".") %>% 
  left_join(glottolog)

#reading in Grambank
GB_df_desc <- read_csv("data/parameters_binary.csv") %>% 
  dplyr::select(ID, Grambank_ID_desc) %>% 
  mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, " ", "_"))

GB_df_integers <- read_tsv("data/GB_wide_strict_binarized.tsv") %>% 
  rename(glottocode = Language_ID) %>% 
  dplyr::select(glottocode, GB_df_desc$ID) %>% 
  melt() %>%
  mutate(value = as.character(value)) %>%  
  mutate(value = str_replace_all(value, "1", "2")) %>%  
  mutate(value = str_replace_all(value, "0", "1")) %>% 
  mutate(value = as.integer(value)) %>% 
  dcast(glottocode ~ variable)


GB_df_all_na_prop <- read_tsv("data/GB_wide_strict_binarized.tsv") %>% 
  dplyr::select(glottocode = Language_ID, na_prop)

GB_oceanic_df <- glottolog %>% 
  filter(str_detect(Path, "ocea1241")) %>% 
  inner_join(GB_df_integers) %>% 
  inner_join(GB_df_all_na_prop) %>% 
  filter(!is.na(na_prop)) %>% 
  inner_join(Glottolog_tree_full_tips_df) 

Glottolog_tree_pruned_oceanic <- drop.tip(Glottolog_tree_full,Glottolog_tree_full$tip.label[-match(GB_oceanic_df$glottocode, Glottolog_tree_full$tip.label)])

GB_oceanic_df_nanggu <- GB_oceanic_df   %>% 
  filter(Name_stripped_no_spaces == "Nanggu") %>% 
  dplyr::select(GB_df_desc$ID) %>% 
  melt() %>% 
  filter(!is.na(value)) %>% 
  dplyr::select(Feature_ID = variable)

###Glottolog parsimony
GB_ACR_all_parsimony <- readRDS("output/ASR/glottolog_tree_binary/parsimony/GB_ACR_all.rds") %>% 
  inner_join(GB_oceanic_df_nanggu)

asr_parsimony_br_len <- function(ASR_object){

#ASR_object <- GB_ACR_all_parsimony$content[[78]]

feature <-  ASR_object[[1]]
asr_object <-  ASR_object[[2]]
feat_vector <- ASR_object[[3]] %>% as.factor()
tree <-  ASR_object[[4]]

feat_vector_named <- as.character(feat_vector)
names(feat_vector_named) <- names(feat_vector)
tree$tip.label <- names(feat_vector)

tree <- root(tree , outgroup = "nang1262")
 
phydat_data <- feat_vector_phydat <-  as.phyDat(feat_vector_named, type = "USER", levels = c("1", "2"))

acctran_tree <- phangorn::acctran(tree = tree, data = phydat_data) 

tabulate(tree$edge[, 1])[nTips(tree) + 1] > 2

df_dist_roots <- distRoot(acctran_tree) %>% as.data.frame()

df_dist_roots_nNodes <- distRoot(acctran_tree, method = "nNodes") %>% as.data.frame() %>% 
  rename(nNodes_dist = ".")


df_dist_roots$glottocode <- names(feat_vector)

df_dist_roots$Grambank_ID <- feature

cat("I'm at ", feature, ".\n")

df_dist_roots %>% 
  cbind(df_dist_roots_nNodes) %>% 
  rename("Cost" = ".") 
}


df <- lapply(GB_ACR_all_parsimony$content, asr_parsimony_br_len) %>% bind_rows()

df_summarised <- df %>% 
  group_by(glottocode) %>% 
  summarise(rowmeans = mean(Cost), nNodes_mean = mean(nNodes_dist))

df_summarised %>% 
  dplyr::select(glottocode, `Mean parsimony cost` = rowmeans, nNodes_mean) %>% 
  write_tsv("output/ASR/conservatism_parsimony_glottolog.tsv")



#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#read in
glottolog <- read_tsv("data/Glottolog_lookup_table_Heti_edition.tsv")  %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) 


df_wide_with_lat_long <- df_summarised %>% 
  dplyr::select(glottocode, `Mean parsimony cost` = rowmeans) %>% 
  left_join(glottolog)


#Basemap
basemap <- ggplot(df_wide_with_lat_long) +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", size = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", size = 0.3)  + 
  theme(panel.grid.major = element_blank(), #all of these lines are just removing default things like grid lines, axises etc
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"), 
        axis.text.x = element_blank(),  
        axis.text.y = element_blank(),  
        axis.ticks = element_blank())   +
  coord_map(projection = "vandergrinten") +
  #  coord_map(projection = "vandergrinten", xlim = c(130, 255), ylim = c(-56, 27)) +
  xlim(c(110, 255)) +
  ylim(c(-56, 27))



basemap + 
  geom_point(aes(x=Longitude, y=Latitude, color =  `Mean parsimony cost`), size = 4, alpha = 0.8) +
  scale_color_gradientn(colours=viridis(n = 20)) +
  theme(legend.text=element_text(size=18), 
        legend.title = element_text(size = 20), 
        legend.key.size = unit(1.6, "cm"))

ggsave(filename = "output/ASR/map_parsimony_cost_glottolog_tree.png", width = 20, height = 11)
