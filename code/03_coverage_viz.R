source("01_requirements.R")

glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv") %>% 
  dplyr::select(Glottocode, classification, Name, level, med, Language_level_ID, Longitude, Latitude) %>% 
  filter(str_detect(classification, "ocea1241")) %>% 
  filter(level == "language") %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

##TREE gray et al mcct
gray_tree <- read.newick(file.path("data", "trees", "gray_et_al_tree_pruned_newick_mmct.txt"))

##TREE glottolog tree
Glottolog_tree_full <- read.tree("data/trees/glottolog_4.3_tree_newick.txt")

GB_df <- read_tsv("data/GB/GB_wide_binarised.tsv") %>% 
  column_to_rownames("Language_ID")

GB_df$na_prop <- apply(GB_df, 1, function(x) mean(is.na(x)))

#marking tip values in glottolog df
glottolog_df_tip_values <- GB_df %>% 
  rownames_to_column("Glottocode") %>% 
  dplyr::select(Glottocode, na_prop) %>% 
  mutate(tip_value = if_else(na_prop < 0.5 , "More than half of features covered in GB", "Less than half of features covered in GB")) %>% 
  full_join(glottolog_df, by = "Glottocode") %>% 
  mutate(tip_value = if_else(str_detect(med, "grammar") & is.na(tip_value), "grammar exists (not in GB, yet)", tip_value)) %>% 
  mutate(tip_value = if_else(!str_detect(med, "grammar") & is.na(tip_value), "grammar doesn't exist", tip_value)) %>% 
  mutate(tip_color = if_else(tip_value == "More than half of features covered in GB", "#0b8c1f", "NA")) %>% 
mutate(tip_color = if_else(tip_value == "Less than half of features covered in GB", "#81f093", tip_color)) %>% 
  mutate(tip_color = if_else(tip_value == "grammar exists (not in GB, yet)", "#DA33FF", tip_color)) %>% 
  mutate(tip_color = if_else(tip_value == "grammar doesn't exist", "#e0421b", tip_color)) 

color_vector <- c( "#e0421b", "#DA33FF", "#81f093","#0b8c1f")

##making map
#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#Basemap
basemap <- ggplot(glottolog_df_tip_values) +
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
  geom_jitter(data = glottolog_df_tip_values, aes(x = Longitude, y = Latitude, 
                                                  color = tip_value),
              #colour = glottolog_df_tip_values$tip_color, 
              alpha = 0.5, shape = 17, width = 1) +
  scale_discrete_manual(aesthetics = c("color"), values = color_vector) +
  labs(color='Coverage') +
  theme(legend.position= c(0.8, 0.3))

ggsave("output/other_plots/coverage_map_oceanic.png", height = 8, width = 15)

