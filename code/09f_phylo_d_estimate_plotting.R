source("01_requirements.R")

full_df <- read_tsv("output/HL_comparison/phylo_d_table.tsv") %>% 
  mutate(one_is_one = ifelse(ones == 1 |zeroes== 1, "yes", "no")) %>% 
  unite(Feature, tree, col = "Feature_tree", remove = F) %>% 
  mutate(min = ifelse(ones < zeroes, ones, zeroes)) %>% 
  mutate(min_p = min / (ones + zeroes)) %>% 
  mutate(Destimate_abs = round(abs(Destimate), 4)) 

lowest_d_not_one_is_one <- full_df %>% 
  filter(min != 1) %>% 
  slice_min(Destimate) %>% 
  dplyr::select(Destimate) %>% 
  as.matrix() %>% 
  as.vector()
  
#full_df$Destimate <- ifelse(full_df$min == 1 & full_df$Destimate_abs >7, lowest_d_not_one_is_one, full_df$Destimate)

full_df %>% 
  filter(!is.na(Destimate))  %>% 
  filter(min != 1) %>% 
  filter(min < 20) %>% 
  ggplot() +
  geom_point(mapping = aes(x = min, y = Destimate, color = min)) +
  theme(legend.position = 0)


full_df$Destimate_abs %>% max(na.rm = T)

full_df %>% 
  ggplot() +
  geom_boxplot(aes(y = Destimate_abs, x = one_is_one) )
