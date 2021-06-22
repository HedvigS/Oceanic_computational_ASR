source("1_requirements.R")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

subregions <- read_tsv("data/oceania_subregions.tsv") %>% 
  rename(glottocode = Glottocode)  %>% 
  mutate(Smallest_Island_group = str_split(Smallest_Island_group, ",")) %>% 
  unnest(Smallest_Island_group) %>% 
  mutate(Smallest_Island_group = trimws(Smallest_Island_group)) %>% 
  dplyr::select(Smallest_Island_group, glottocode) 


#dates <- read_xlsx("../data/Remote_Oceania_Political_complex_and_more/island_group_settlement_date.xlsx") %>% dplyr::select(Smallest_Island_group = "Smaller specific island group", Time_depth = "Time depth settlement group") %>% 
 # full_join(subregions) 

#parsimony_gray <- read_tsv("output/ASR/conservatism_parsimony_gray.tsv") %>% 
#  mutate(`Mean parsimony cost` = range01(`Mean parsimony cost`)) %>% 
#  rename(parsimony_cost_gray = `Mean parsimony cost`) %>% 
#  rename(taxon = glottocode) %>% 
#  left_join(taxa) %>% 
#  dplyr::select(glottocode, parsimony_cost_gray)

#parsimony_glottolog <- read_tsv("output/ASR/conservatism_parsimony_glottolog.tsv") %>% 
#  mutate(`Mean parsimony cost` = range01(`Mean parsimony cost`)) %>% 
#  rename(parsimony_cost_glottolog = `Mean parsimony cost`) %>% 
#  dplyr::select(parsimony_cost_glottolog, glottocode)

ML_gray <- read_tsv("output/gray_et_al_2009/ML/mcct/conservatism_ML_gray.tsv") %>% 
  mutate(mean_dist = range01(mean_dist)) %>% 
  rename(ML_dist_gray = mean_dist) %>% 
  dplyr::select(ML_dist_gray, glottocode)

ML_glottolog <- read_tsv("output/glottolog_tree_binary/ML/conservatism_ML_glottolog.tsv") %>% 
  mutate(mean_dist = range01(mean_dist)) %>% 
  rename(ML_dist_glottolog = mean_dist) %>% 
  dplyr::select(ML_dist_glottolog , glottocode)


all_dists_wide <- ML_glottolog %>% 
  full_join(ML_gray) %>% 
#  full_join(parsimony_glottolog) %>% 
#  full_join(parsimony_gray) %>% 
#  rename(`Mean parsimony cost (Glottolog)` = parsimony_cost_glottolog) %>% 
#  rename(`Mean parsimony cost (Gray et al 2009)` = parsimony_cost_gray) %>% 
  rename(`Mean ML-branch length (Gray et al 2009)` = ML_dist_gray) %>% 
  rename(`Mean ML-branch length (Glottolog)` = ML_dist_glottolog) 


all_dists <- ML_glottolog %>% 
  full_join(ML_gray) %>% 
#  full_join(parsimony_glottolog) %>% 
 # full_join(parsimony_gray) %>% 
#  rename(`Mean parsimony cost (Glottolog)` = parsimony_cost_glottolog) %>% 
#  rename(`Mean parsimony cost (Gray et al 2009)` = parsimony_cost_gray) %>% 
  rename(`Mean ML-branch length (Gray et al 2009)` = ML_dist_gray) %>% 
  rename(`Mean ML-branch length (Glottolog)` = ML_dist_glottolog) %>% 
  reshape2::melt(id.vars = "glottocode") %>% 
  filter(!is.na(value))

glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())   %>% 
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))  %>% 
  rename(glottocode = Glottocode)

df_wide_with_lat_long <- all_dists %>% 
  left_join(glottolog_df)   

#worldmaps
#rendering a worldmap that is pacific centered
world <- map_data('world', wrap=c(-25,335), ylim=c(-56,80), margin=T)

lakes <- map_data("lakes", wrap=c(-25,335), col="white", border="gray", ylim=c(-55,65), margin=T)



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
  geom_point(aes(x=Longitude, y=Latitude, color =  value), size = 4, alpha = 0.8) +
  scale_color_gradientn(colours=viridis(n = 20)) +
  theme(legend.text=element_text(size=18), 
        legend.title = element_text(size = 20), 
        legend.key.size = unit(1.6, "cm"), 
        legend.position = "None", 
        strip.text = element_text(size=25), 
        strip.background=element_rect(fill="#dcfaff")) +
  facet_wrap(~variable, ncol = 2)

ggsave("output/other_plots/br_len_maps.png", width = 26, height =20)

##SPLOM TIME

all_dists_for_SPLOM <- ML_glottolog %>% 
  full_join(ML_gray) #%>% 
#  full_join(parsimony_glottolog) %>% 
#  full_join(parsimony_gray) %>% 
#  left_join(dates) %>% 
#  dplyr::select(-Smallest_Island_group) %>% 
#  rename("ML\nGlottolog-tree" = "ML_dist_glottolog",
#  "ML\nGray et al 2009-tree"= "ML_dist_gray", 
#  "Parsimony\nGlottolog-tree" = "parsimony_cost_glottolog" , 
#  "Parsimony\nGray et al 2009-tree"= "parsimony_cost_gray",
#  "Settlement time order" = "Time_depth" )


png("output/other_plots/br_len_splom.png",  height =17, width = 17, units = "cm", res = 400)

pairs.panels(all_dists_for_SPLOM[,c(1, 3)], 
             method = "pearson", # correlation method
             hist.col = "#a3afd1",# "#a9d1a3","",""),
             density = TRUE,  # show density plots
             ellipses = F, # show correlation ellipses
             cex = 0.25,
             #           smoother= T,
             cor=T,
             lm=T,
#             labels = c("Maximum Likelihood\nGlottolog-tree", "Maximum Likelihood\nGray et al 2009-tree", "Parsimony\nGlottolog-tree", "Parsimony\nGray et al 2009-tree", "Settlement time depth"),
             ci = T, cex.cor = 0.9,stars = T)

dev.off()


#ggpairs(all_dists_for_SPLOM[,2:6], ) +
#  theme_classic()


###COMPARING TO POL COMPLEX DATA


pol_complex <-  read_csv("../data/Remote_Oceania_Political_complex_and_more/Remote_oceania_pol_complex_hedvig_code.csv") %>% 
  dplyr::select(Smallest_Island_group = Smallest_Island_group_main, `Pol complex` = pol_complex_code_Hedvig) %>% 
  full_join(subregions) 



all_dists_for_SPLOM_pol_complex <- all_dists_for_SPLOM  %>% 
  left_join(pol_complex) %>% 
  left_join(dates) %>% 
  dplyr::select(-Smallest_Island_group) %>% 
  distinct() 

png("output/ASR/other_plots/br_len_splom_with_pol_complex_dates.png",  height =25, width = 25, units = "cm", res = 400)

pairs.panels(all_dists_for_SPLOM_pol_complex [,2:7], 
             method = "pearson", # correlation method
             hist.col = "#a3afd1",# "#a9d1a3","",""),
             density = TRUE,  # show density plots
             ellipses = F, # show correlation ellipses
             cex = 0.25,
             #           smoother= T,
             cor=T,
             lm=T,
             ci = T, cex.cor = 0.9,stars = T)
dev.off()

#subregions
subregions <- read_tsv("../data/Remote_Oceania_Political_complex_and_more/oceania_subregions.tsv") %>% 
  rename(glottocode = Glottocode)

all_dists_grouped_by_aberr <- all_dists %>%
  left_join(subregions)  %>% 
  group_by(Abberancy_group, variable) %>% 
  summarise(value = mean(value, na.rm = T)) %>%
  arrange(variable, value)

all_dists_grouped_by_aberr %>% 
  filter(variable == "Mean ML-branch length (Glottolog)") %>% 
  dplyr::select(`Island group` = Abberancy_group, `Average distance from root (Proto-Oceanic)` = value) %>% xtable(caption = "Island groups ordered by conservatism (ML Glottolog)", label = "conservatism_group_ML_glottolog") %>% 
  xtable::print.xtable(include.rownames = F) %>% 
  write_lines("output/ASR/conservatism_group_ML_glottolog.txt")

all_dists_grouped_by_aberr %>% 
  filter(variable == "Mean ML-branch length (Gray et al 2009)") %>% 
  dplyr::select(`Island group` = Abberancy_group, `Average distance from root (Proto-Oceanic)` = value) %>% xtable(caption = "Island groups ordered by conservatism (ML Gray)", label = "conservatism_group_ML_Gray") %>% 
  xtable::print.xtable(include.rownames = F) %>% 
  write_lines("output/ASR/conservatism_group_ML_Gray.txt")

all_dists_grouped_by_aberr %>% 
  filter(variable == "Mean parsimony cost (Glottolog)") %>% 
  dplyr::select(`Island group` = Abberancy_group, `Average distance from root (Proto-Oceanic)` = value) %>% xtable(caption = "Island groups ordered by conservatism (Parsimony Glottolog)", label = "conservatism_group_parsimony_glottolog") %>% 
  xtable::print.xtable(include.rownames = F) %>% 
  write_lines("output/ASR/conservatism_group_parsimony_glottolog.txt")

all_dists_grouped_by_aberr %>% 
  filter(variable == "Mean parsimony cost (Gray et al 2009)") %>% 
  dplyr::select(`Island group` = Abberancy_group, `Average distance from root (Proto-Oceanic)` = value) %>% xtable(caption = "Island groups ordered by conservatism (Parsimony Gray)", label = "conservatism_group_parsimony_Gray") %>% 
  xtable::print.xtable(include.rownames = F) %>% 
  write_lines("output/ASR/conservatism_group_parsimony_Gray.txt")

all_dists_grouped_by_aberr %>% 
  group_by(Abberancy_group) %>% 
  summarise(value = mean(value, na.rm = T)) %>%
  arrange(value) %>%
  dplyr::select(`Island group` = Abberancy_group, `Average distance from root (Proto-Oceanic)` = value) %>% xtable(caption = "Island groups ordered by conservatism (Parsimony Gray)", label = "conservatism_group_parsimony_Gray") %>% 
  xtable::print.xtable(include.rownames = F) %>% 
  write_lines("output/ASR/conservatism_group_all.txt")



###ridgplots

grouped <- all_dists_grouped_by_aberr %>% 
  group_by(Abberancy_group) %>% 
  summarise(group_mean_value = mean(value, na.rm = T)) 

dists_for_ridgeplot <-  all_dists %>% 
  left_join(subregions)  %>% 
  left_join(grouped)

dists_for_ridgeplot$Abberancy_group <- fct_reorder(dists_for_ridgeplot$Abberancy_group, -dists_for_ridgeplot$group_mean_value)


mean_labels <- dists_for_ridgeplot %>% 
  group_by(variable, Abberancy_group) %>% 
  summarise(mean_dist = mean(value))




dists_for_ridgeplot %>% 
  dplyr::select(value, variable, Abberancy_group) %>% 
  ggplot(aes(x = value, y = Abberancy_group, fill = Abberancy_group)) +
  geom_density_ridges(panel_scaling = F, quantile_lines = T, quantile_fun = mean, jittered_points = TRUE, point_size = 2, point_shape = 21  ,  position = position_points_jitter(height = 0)) +
  geom_label(data = mean_labels, aes(x = mean_dist, y = Abberancy_group,
                                        label = round(mean_dist, 2)), size = 2, nudge_x = 0.01, nudge_y = 0.2, alpha = 0.7, label.padding = unit(0.1, "lines")) +
  theme_classic() +
  xlim(c(0,1)) +
  theme(axis.title = element_blank(), 
        legend.position = "None") +
  facet_wrap(~variable)

ggsave("output/ASR/other_plots/br_len_ridgeplots.png", width = 10, height =10)


mean_labels_ML <- dists_for_ridgeplot %>% 
  filter(str_detect(variable, "ML")) %>% 
  group_by(variable, Abberancy_group) %>% 
  summarise(mean_dist = mean(value))




dists_for_ridgeplot %>% 
  filter(str_detect(variable, "ML")) %>% 
  dplyr::select(value, variable, Abberancy_group) %>% 
  ggplot(aes(x = value, y = Abberancy_group, fill = Abberancy_group)) +
  geom_density_ridges(panel_scaling = F, quantile_lines = T, quantile_fun = mean, jittered_points = TRUE, point_size = 2, point_shape = 21  ,  position = position_points_jitter(height = 0)) +
  geom_label(data = mean_labels_ML, aes(x = mean_dist, y = Abberancy_group,
                                     label = round(mean_dist, 2)), size = 2, nudge_x = 0.01, nudge_y = 0.2, alpha = 0.7, label.padding = unit(0.1, "lines")) +
  theme_classic() +
  xlim(c(0,1)) +
  theme(axis.title = element_blank(), 
        legend.position = "None") +
  facet_wrap(~variable)

ggsave("output/ASR/other_plots/br_len_ridgeplots_ML.png", width = 10, height =5)



mean_labels_MP <- dists_for_ridgeplot %>% 
  filter(str_detect(variable, "pars")) %>% 
  group_by(variable, Abberancy_group) %>% 
  summarise(mean_dist = mean(value))



dists_for_ridgeplot$Abberancy_group <- fct_reorder(dists_for_ridgeplot$Abberancy_group, -dists_for_ridgeplot$group_mean_value)

dists_for_ridgeplot %>% 
  filter(str_detect(variable, "pars")) %>% 
  dplyr::select(value, variable, Abberancy_group) %>% 
  ggplot(aes(x = value, y = Abberancy_group, fill = Abberancy_group)) +
  geom_density_ridges(panel_scaling = F,  quantile_lines = T, quantile_fun = mean, jittered_points = TRUE, point_size = 2, point_shape = 21  ,  position = position_points_jitter(height = 0)) +
  geom_label(data = mean_labels_MP, aes(x = mean_dist, y = Abberancy_group,
                                        label = round(mean_dist, 2)), size = 2, nudge_x = 0.01, nudge_y = 0.2, alpha = 0.7, label.padding = unit(0.1, "lines")) +
  theme_classic() +
  xlim(c(0,1)) +
  theme(axis.title = element_blank(), 
        legend.position = "None") +
  facet_wrap(~variable)

ggsave("output/ASR/other_plots/br_len_ridgeplots_MP.png", width = 10, height =5)



#LM stuff

for_lm <- all_dists %>%
#  filter(variable == "Mean ML-branch length (Glottolog)") %>% 
#  filter(variable == "Mean ML-branch length (Gray et al 2009)") %>% 
#  filter(variable == "Mean parsimony cost (Gray et al 2009)") %>% 
#  filter(variable == "Mean parsimony cost (Glottolog)") %>% 
  group_by(glottocode) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  left_join(subregions) 


for_lm$centered_value  <- for_lm$value  -mean(for_lm$value, na.rm = T) 
  
model <- lm(value ~ Abberancy_group, for_lm)

summary(model)

model$residuals %>% hist()

