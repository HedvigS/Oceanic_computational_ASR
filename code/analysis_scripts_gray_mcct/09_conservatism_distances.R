source("01_requirements.R")

#reading in glottolog language table (to be used for language names for plot and to pre-filter out non-oceanic
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Glottocode, classification, level)

#GB_data
GB_df_oceanic <- read_tsv("data/GB/GB_wide_binarised.tsv", col_types = cols()) %>% 
  rename(Glottocode = Language_ID) %>% 
  left_join(glottolog_df, by = "Glottocode") %>% 
  filter(str_detect(classification, "ocea1241")) %>%
  filter(level != "family") %>% 
  dplyr::select(-classification, -level)
  
#reconstructions
df <- read_tsv("output/conservatism/all_reconstructions.tsv")

#taking the resconstructions and making them each into a separate melted dataframe, to be merged later with the rest and run distances on
#this part could be effectivised by a function, I just haven't done it yet

#Proto-eastern Polynesian
proto_east_poly <- df %>% 
  filter(`Proto-language` == "Proto-Eastern Polynesian") %>% 
  dplyr::select(Feature_ID, `Proto-Eastern Polynesian (HL)` = Prediction, `Proto-Eastern Polynesian (Gray, ML)` = gray_ML_prediction_1, `Proto-Eastern Polynesian (Gray, MP)` = gray_parsimony_prediction_1, `Proto-Eastern Polynesian (Glottolog, ML)` = glottolog_ML_prediction_1, `Proto-Eastern Polynesian (Glottolog, MP)` = glottolog_parsimony_prediction_1) %>% 
  t() 

colnames(proto_east_poly) <- proto_east_poly[1,]
proto_east_poly <- proto_east_poly[2:6,] 

proto_east_poly <- proto_east_poly %>% 
  as.data.frame() %>% 
  rownames_to_column("Glottocode") %>% 
  reshape2::melt(id.vars = "Glottocode") %>% 
  mutate(value = as.numeric(value)) 
  
#proto Polynesian
proto_poly <- df %>%
  filter(`Proto-language` == "Proto-Polynesian") %>% 
  dplyr::select(Feature_ID, `Proto-Polynesian (HL)` = Prediction, `Proto-Polynesian (Gray, ML)` = gray_ML_prediction_1, `Proto-Polynesian (Gray, MP)` = gray_parsimony_prediction_1, `Proto-Polynesian (Glottolog, ML)` = glottolog_ML_prediction_1, `Proto-Polynesian (Glottolog, MP)` = glottolog_parsimony_prediction_1) %>% 
  t() 

colnames(proto_poly) <- proto_poly[1,]
proto_poly <- proto_poly[2:6,] 

proto_poly <- proto_poly %>% 
  as.data.frame() %>% 
  rownames_to_column("Glottocode") %>% 
  reshape2::melt(id.vars = "Glottocode") %>% 
  mutate(value = as.numeric(value)) 

#proto central Pacific
proto_cp <- df %>% 
  filter(`Proto-language` == "Proto-Central Pacific") %>% 
    dplyr::select(Feature_ID, `Proto-Central Pacific (HL)` = Prediction, `Proto-Central Pacific (Gray, ML)` = gray_ML_prediction_1, `Proto-Central Pacific (Gray, MP)` = gray_parsimony_prediction_1, `Proto-Central Pacific (Glottolog, ML)` = glottolog_ML_prediction_1, `Proto-Central Pacific (Glottolog, MP)` = glottolog_parsimony_prediction_1) %>% 
  t() 

colnames(proto_cp) <- proto_cp[1,]
proto_cp <- proto_cp[2:6,] 

proto_cp <- proto_cp %>% 
  as.data.frame() %>% 
  rownames_to_column("Glottocode") %>% 
  reshape2::melt(id.vars = "Glottocode") %>% 
  mutate(value = as.numeric(value)) 


#proto Oceanic
proto_oceanic <- df %>% 
  filter(`Proto-language` == "Proto-Oceanic") %>% 
  dplyr::select(Feature_ID, `Proto-Oceanic (HL)` = Prediction, `Proto-Oceanic (Gray, ML)` = gray_ML_prediction_1, `Proto-Oceanic (Gray, MP)` = gray_parsimony_prediction_1, `Proto-Oceanic (Glottolog, ML)` = glottolog_ML_prediction_1, `Proto-Oceanic (Glottolog, MP)` = glottolog_parsimony_prediction_1) %>% 
  t() 

colnames(proto_oceanic) <- proto_oceanic[1,]
proto_oceanic <- proto_oceanic[2:6,] 

proto_oceanic <- proto_oceanic %>% 
  as.data.frame() %>% 
  rownames_to_column("Glottocode") %>% 
  reshape2::melt(id.vars = "Glottocode") %>% 
  mutate(value = as.numeric(value)) 

##joining with GB general

proto_langs_plus_descendants <- GB_df_oceanic %>% 
  reshape2::melt(id.vars = "Glottocode") %>% 
  full_join(proto_east_poly, by = c("Glottocode", "variable", "value")) %>% 
    full_join(proto_poly, by = c("Glottocode", "variable", "value")) %>% 
  full_join(proto_cp, by = c("Glottocode", "variable", "value")) %>%
    full_join(proto_oceanic, by = c("Glottocode", "variable", "value")) %>% 
  reshape2::dcast(Glottocode ~ variable) %>% 
  column_to_rownames("Glottocode") %>% 
  as.matrix()

#doing the distances
outcome<- cluster::daisy(proto_langs_plus_descendants, metric = "gower")

#organising the languages into groups


#reading in data which specifies which island is connected with which language
subregions <- read_tsv("data/oceania_subregions.tsv") %>% 
  full_join(read_tsv("data/oceania_subregions_extra.tsv") ) %>% 
  filter(!is.na(Abberancy_group)) %>% 
  distinct(Glottocode, Abberancy_group) %>%
  left_join(glottolog_df) %>% 
  mutate(group = if_else(str_detect(classification, "east2449"), "Eastern Polynesian", Abberancy_group)) %>% 
 mutate(group = if_else(str_detect(classification, "poly1242") & !str_detect(classification, "east2449"), "Polynesian (non-eastern)", group)) %>% 
  mutate(group = if_else(str_detect(classification, "cent2060") & !str_detect(classification, "poly1242"), "Central Pacific (non-Polynesian)", group)) %>% 
  right_join(GB_df_oceanic)

subregions_left <- subregions %>% 
  dplyr::select(Var1 = Glottocode, group_var1 = group)
  
subregions_right <- subregions %>% 
  dplyr::select(Var2 = Glottocode, group_var2 = group)

outcome_grouped <-  outcome %>% 
  as.matrix() %>% 
  reshape2::melt() %>% 
  left_join(subregions_left ) %>% 
  left_join(subregions_right ) %>% 
  mutate(group_var2 = if_else(is.na(group_var2), Var2, group_var2)) %>% 
  mutate(group_var1 = if_else(is.na(group_var1), Var1, group_var1)) %>% 
  group_by(group_var1, group_var2) %>% 
  summarise(mean = mean(value)) 

outcome_grouped_wide <- outcome_grouped %>% 
  reshape2::dcast(group_var1~ group_var2, value.var = "mean") %>% 
  column_to_rownames("group_var1") %>% 
  as.matrix()

heatmap.2(outcome_grouped_wide, 
          key = F, symm = T,
          dendrogram = "none",
          revC = T,
          trace = "none", cellnote = round(outcome_matrix, 2),
          margin=c(20,20), col=viridis::magma(15, direction = -1))


outcome_glottolog_MP <- outcome_grouped  %>% 
  filter(!str_detect(group_var1, "Glottolog, ML")) %>% 
  filter(!str_detect(group_var1, "Gray, ML")) %>% 
  filter(!str_detect(group_var1, "Gray, MP")) %>%
  filter(!str_detect(group_var2, "Glottolog, ML")) %>% 
  filter(!str_detect(group_var2, "Gray, ML")) %>% 
  filter(!str_detect(group_var2, "Gray, MP")) %>% 
  reshape2::dcast(group_var1~ group_var2, value.var = "mean") %>% 
  column_to_rownames("group_var1") %>% 
  as.matrix()


heatmap.2(outcome_glottolog_MP, 
          key = F, symm = T,
          dendrogram = "none",
          revC = T,
          trace = "none", cellnote = round(outcome_glottolog_MP, 2),
          margin=c(20,20), col=viridis::magma(20))






