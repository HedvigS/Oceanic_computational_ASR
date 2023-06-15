source("01_requirements.R")

glottolog_df <- read_tsv("output/processed_data/glottolog_language_table_wide_df.tsv") %>% 
  dplyr::select(Glottocode, classification, Name, level, med, Language_level_ID, Longitude, Latitude, Countries) %>% 
  filter(str_detect(classification, "ocea1241")) %>% 
  filter(level == "language") %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude)) %>% 
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID))

##TREE gray et al mcct
gray_tree_fn <-"output/processed_data/trees/gray_et_al_tree_pruned_newick_mcct.txt"
if(!file.exists(gray_tree_fn)){
  source("analysis_scripts_gray_mcct/03_get_gray_tree_mcct.R")
  }
gray_tree <- ape::read.tree(gray_tree_fn) 

##TREE glottolog tree
Glottolog_tree_full <- read.tree("output/processed_data/trees/glottolog_tree_newick_all_oceanic.txt") 

GB_df_desc <- read_tsv(GB_df_desc_fn) %>% 
  filter(Binary_Multistate != "Multi")

#reading in GB
GB_df <- read_tsv(GB_binary_fn)

#marking tip values in glottolog df
glottolog_df_tip_values <- GB_df %>% 
  dplyr::select(Glottocode = Language_ID, na_prop) %>% 
  mutate(tip_value = if_else(na_prop < 0.5 , "More than half of features covered in GB", "Less than half of features covered in GB")) %>% 
  right_join(glottolog_df, by = "Glottocode") %>% 
  mutate(tip_value = if_else(str_detect(med, "grammar") & is.na(tip_value), "grammar exists (not in GB, yet)", tip_value)) %>% 
  mutate(tip_value = if_else(!str_detect(med, "grammar") & is.na(tip_value), "grammar doesn't exist", tip_value)) %>% 
  mutate(tip_color = if_else(tip_value == "More than half of features covered in GB", "#0b8c1f", "NA")) %>% 
 mutate(tip_color = if_else(tip_value == "Less than half of features covered in GB", "#81F093", tip_color)) %>% 
  mutate(tip_color = if_else(tip_value == "grammar exists (not in GB, yet)", "#7D81F5", tip_color)) %>% 
  mutate(tip_color = if_else(tip_value == "grammar doesn't exist", "#FFB87A", tip_color)) %>% 
  filter(Glottocode != "poly1242") %>% #remove proto-languages to reduce confusion 
  filter(Glottocode != "east2449") %>% 
  filter(Glottocode != "cent2060") 

color_vector_tree <- c( "#FFB87A", "#7D81F5", "#81f093","#0b8c1f")
color_vector_map <- c(colours_binary, "#c9c9c9")

###COVERAGE PLOT: MAP

#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)
lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)

#Basemap
basemap <- ggplot(glottolog_df_tip_values) +
  geom_polygon(data=world, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="gray87", linewidth = 0.5) +
  geom_polygon(data=lakes, aes(x=long, 
                               y=lat,group=group),
               colour="gray87", 
               fill="white", linewidth = 0.3)  + 
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
  scale_x_continuous(limits = c(110, 255) , expand=c(0,0)) +
  scale_y_continuous(limits = c(-56, 27) , expand=c(0,0))


#map plot for coverage of oceanic languages
png(paste0(OUTPUTDIR_plots, "/coverage_plots/maps/coverage_map_oceanic.png"), height = 6, width = 10, units = "in", res = 300)

basemap +
  geom_jitter(data = filter(glottolog_df_tip_values, !is.na(Longitude)), aes(x = Longitude, y = Latitude, 
                                                  color = tip_value),
              alpha = 0.5, shape = 17, width = 2) +
  scale_discrete_manual(aesthetics = c("color"), values = color_vector_tree) +
  labs(color='Coverage') +
  theme(legend.position= c(0.8, 0.3)) 
  

x <- dev.off()

###Maps per feature

###MAPS PER FEATURE
#Basemap

#check if maps already exists, if so don't make 'em again
FN <- paste0(OUTPUTDIR_plots, "coverage_plots/maps/map_", GB_df_desc$ID[sample(1:nrow(GB_df_desc), size = 1)], ".png")
if(!file.exists(FN)){

  for(feature in GB_df_desc$ID){

#  feature <- GB_df_desc$ID[1]
  
df_for_plot <- GB_df %>%
    rename(Glottocode = Language_ID) %>% 
    inner_join(glottolog_df, by = "Glottocode") %>% 
    dplyr::select(Glottocode, Value = {{feature}}, Longitude, Latitude) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Value = as.character(Value))
  
plot_title <- GB_df_desc %>%
    filter(ID == {{feature}}) %>%
    dplyr::select(Grambank_ID_desc) %>% 
    mutate(Grambank_ID_desc = str_replace_all(Grambank_ID_desc, "_", " ")) %>% 
    as.matrix() %>% 
    as.vector()

FN <- paste0(OUTPUTDIR_plots, "coverage_plots/maps/map_", feature, ".png")

plot <-   basemap + 
  geom_jitter(data = df_for_plot, aes(x=Longitude, y=Latitude, fill = Value),  size = 1.5 , alpha = 0.8, shape = 21, stroke = 0.2) +
  scale_discrete_manual(aesthetics = "fill", values = color_vector_map) +
  ggtitle(plot_title) +
  theme(legend.title = element_blank())


png(FN,  height = 5, width = 7, units = "in" , res = 300)
plot(plot)
x <- dev.off()

  cat(paste0("Done with map plot for ", feature, ".\n"))
}
}

###COVERAGE PLOT: TREES
##Gray et al

gray_tree_tip_value_df <- gray_tree$tip.label %>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df_tip_values, by = "Glottocode") 

gray_tree$tip.label <- gray_tree_tip_value_df$Name

x <- gray_tree_tip_value_df$tip_value

png(file = paste0(OUTPUTDIR_plots, "/coverage_plots/tree/Oceanic_tree_desc_status_gray_et_al_tree_mcct.png"), width = 8.27, height = 10.69, units = "in", res = 600)

plot.phylo(ladderize(gray_tree , right = F), col="grey", tip.color = gray_tree_tip_value_df$tip_color, type = "fan", cex = 0.7,label.offset = 0.1)

lastPP<-get("last_plot.phylo",env=.PlotPhyloEnv)
ss<-sort(unique(x))
par(fg="black")
colors<-setNames(color_vector_tree[1:length(ss)],ss)
add.simmap.legend(colors=colors,vertical=T,x=-4.5,
                  y=-5.1,prompt=F)

title("Coverage of the Oceanic subgroup in Grambank (Gray et al 2009 MCCT tree)", cex.main = 1, line = 1)

x <- dev.off()


png(file = paste0(OUTPUTDIR_plots, "/coverage_plots/tree/Oceanic_gray_et_al_tree_mcct_bare.png"), width = 8, height = 8, units = "in", res = 600)
par(mar=c(0,0,0,0))
plot.phylo(ladderize(gray_tree , right = T), show.tip.label = F, 
           col="grey", type = "cladogram")
x <- dev.off()

###Glottolog_tree
glottolog_tree_tip_value_df <- Glottolog_tree_full$tip.label%>% 
  as.data.frame() %>% 
  rename(Glottocode = ".") %>% 
  left_join(glottolog_df_tip_values, by = "Glottocode") 

Glottolog_tree_full$tip.label <- glottolog_tree_tip_value_df$Name

x <- glottolog_tree_tip_value_df$tip_value

#Glottolog_tree_full <- compute.brlen(Glottolog_tree_full, method = 1)

png(file = paste0(OUTPUTDIR_plots, "/coverage_plots/tree/Oceanic_tree_desc_status_glottolog_tree.png"), width = 8.27, height = 10.69, units = "in", res = 600)

plot.phylo(ladderize(Glottolog_tree_full , right = F), col="grey", tip.color = glottolog_tree_tip_value_df$tip_color, type = "fan", cex = 0.4,label.offset = 0.05)

lastPP<-get("last_plot.phylo",env=.PlotPhyloEnv)
ss<-sort(unique(x))
par(fg="black")
colors<-setNames(color_vector_tree[1:length(ss)],ss)
add.simmap.legend(colors=colors,
                  vertical=T,
                  x=-10.5,
                  y=-10.1,prompt=F)
title("Coverage of the Oceanic subgroup in Grambank (Glottolog 4.0-tree)", cex.main = 1, line = 1)

x <- dev.off()

png(file = paste0(OUTPUTDIR_plots, "/coverage_plots/tree/Oceanic_glottolog_tree_bare.png"), width = 8, height = 8, units = "in", res = 600)

par(mar=c(0,0,0,0))
plot.phylo(ladderize(Glottolog_tree_full , right = T), show.tip.label = F, 
           col="grey", type = "cladogram")

x <- dev.off()


#table with coverage stats

island_groups_df <-   read_tsv("data/island_groups.tsv") %>% 
  right_join(  glottolog_df_tip_values, by = "Glottocode") 

island_groups_table <- island_groups_df %>% 
  filter(!is.na(`Island group`)) %>% 
  group_by(`Island group`, tip_value) %>% 
  summarise(n = n()) %>% 
  reshape2::dcast(`Island group`~ tip_value, value.var = "n") 

island_groups_table[is.na(island_groups_table)] <- 0

island_groups_table <-   island_groups_table %>% 
  janitor::adorn_totals("row") 

#xtable making

cap <- "Table showing coverage of Oceanic languages in Grambank per island group."
lbl <- "GB_coverage_table_island_group"
align <- c("p{3cm}", "p{5cm}","p{2.5cm}", "p{2.5cm}", "p{2.5cm}","p{2.5cm} ") 

island_groups_table_latex_formatting <- island_groups_table %>%
  dplyr::select("Island group", "More than half of features covered in GB", "Less than half of features covered in GB", "grammar exists (not in GB, yet)", "grammar doesn't exist") %>% 
      rename("$\\textbf{\\cellcolor{spec_color_orange!50}{No grammar}}$" = "grammar doesn't exist" ) %>% 
    rename("$\\textbf{\\cellcolor{spec_color_blue!50}{\\parbox{2.7cm}{\\raggedright Grammar exists, but language not in Grambank (yet)}}}$" =   "grammar exists (not in GB, yet)") %>% 
   rename("$\\textbf{\\cellcolor{spec_color_lightgreen!50}{\\parbox{2.7cm}{\\raggedright Less than half of the features covered in Grambank}}}$" = "Less than half of features covered in GB") %>% 
    rename("$\\textbf{\\cellcolor{spec_color_darkgreen!50}{\\parbox{2.7cm}{\\raggedright More than half of the features covered in Grambank}}}$" = "More than half of features covered in GB") %>% 
  rename("$\\textbf{\\parbox{2.7cm}{\\raggedright Island group}}$" = "Island group")

island_groups_table_latex_formatting  %>% 
    xtable(caption = cap, label = lbl,
           digits = 0, 
           align = align) %>% 
  xtable::print.xtable(file = file.path( OUTPUTDIR_plots , "coverage_plots", "tables","island_groups_table.tex"), sanitize.colnames.function = function(x){x},
          include.rownames = FALSE, 
          math.style.negative = F,
          table.placement = "ht",
          booktabs = TRUE, hline.after = c(-1, 0, nrow(island_groups_table_latex_formatting)-1, nrow(island_groups_table_latex_formatting))) 