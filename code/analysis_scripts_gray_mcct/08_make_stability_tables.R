source("01_requirements.R")


#parameter description
GB_df_desc <- read_tsv("data/GB/parameters_binary.tsv", col_types = cols()) %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(Feature_ID = ID, Abbreviation =Grambank_ID_desc)

##making_value_count_df
value_counts_parsimony_glottolog <- read_csv("output/glottolog_tree_binary/parsimony/results.csv") %>%
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_glottolog = min / (`0`+ `1`) %>% round(2))

value_counts_parsimony_gray <- read_csv("output/gray_et_al_2009/parsimony/mcct/results.csv") %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent_parsimony_gray = min / (`0`+ `1`) %>% round(2)) 

value_counts_ML_glottolog <- read_csv("output/glottolog_tree_binary/ML/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_glottolog = (min / nTips) %>% round(2)) 

value_counts_ML_gray <- read_csv("output/gray_et_al_2009//ML/mcct/results.csv") %>%
  mutate(min = pmin( nTips_state_0,  nTips_state_1)) %>% 
  mutate(min_percent_ML_gray = (min / nTips) %>% round(2)) 

#combining
value_count_df <- value_counts_ML_gray %>% 
  full_join(value_counts_ML_glottolog,  by = "Feature_ID") %>%
  full_join(value_counts_parsimony_glottolog,  by = "Feature_ID") %>%
  full_join(value_counts_parsimony_gray,  by = "Feature_ID") 


##########PARSIMONY#########################

##Glottolog

parsimony_glottolog <- value_counts_parsimony_glottolog  %>% 
  filter(ntips > ntips_half_glottolog) %>% 
  inner_join(GB_df_desc, by = "Feature_ID")

parsimony_glottolog_top_stable <- parsimony_glottolog %>% 
  arrange(Parsimony_cost) %>% 
  filter(Parsimony_cost < 2) %>% 
  mutate(parsimony_glottolog_stable = "Top-5")
  
parsimony_glottolog_top_stable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Parsimony cost` = Parsimony_cost, `0`, `1`, Languages = ntips) %>% 
    xtable(digits = 0, caption = c("Most stable features, Parsimony Glottolog-tree."), label = c("stable_parsimony_glottolog")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/parsimony_glottolog_top_stable.txt")

parsimony_glottolog_top_unstable <- parsimony_glottolog %>% 
  arrange(-Parsimony_cost) %>% 
  .[1:5,] %>% 
  arrange(Parsimony_cost) %>% 
  mutate(parsimony_glottolog_unstable = "Top-5")

  parsimony_glottolog_top_unstable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Parsimony cost` = Parsimony_cost, `0`, `1`, Languages = ntips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Parsimony Glottolog-tree."), label = c("unstable_parsimony_glottolog")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/parsimony_glottolog_all_unstable.txt")

parsimony_glottolog_top_5_stable <- parsimony_glottolog %>% 
  arrange(Parsimony_cost) %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent = min / (`0`+ `1`)) %>%
  filter(min_percent > 0.05) %>% 
  .[1:5,] %>% 
  mutate(parsimony_glottolog_top_5_stable = "Top-5")

parsimony_glottolog_top_5_stable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Parsimony cost` = Parsimony_cost, `0`, `1`, Languages = ntips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Parsimony Glottolog-tree, only features with at least a distribution of 95% / 5%."), label = c("stable_parsimony_glottolog_5_percent")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/parsimony_glottolog_5_percent_stable.txt")

###Gray

parsimony_gray <- value_counts_parsimony_gray %>% 
  filter(ntips > ntips_half_gray) %>% 
  inner_join(GB_df_desc, by = "Feature_ID")

parsimony_gray_top_stable <- parsimony_gray %>% 
  arrange(Parsimony_cost) %>% 
  filter(Parsimony_cost < 2) %>% 
  mutate(parsimony_gray_top_stable = "Top-5")

parsimony_gray_top_stable %>%   
  dplyr::select(`Grambank ID` =  Abbreviation, `Parsimony cost` = Parsimony_cost, `0`, `1`, Languages = ntips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Parsimony Gray et al 2009-tree."), label = c("stable_parsimony_gray")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/parsimony_gray_top_stable.txt")

parsimony_gray_unstable <- parsimony_gray %>% 
  arrange(-Parsimony_cost) %>% 
  .[1:5,] %>% 
  arrange(Parsimony_cost) %>% 
  mutate(parsimony_gray_top_unstable = "Top-5")

parsimony_gray_unstable %>%   
  dplyr::select(`Grambank ID` =  Abbreviation, `Parsimony cost` = Parsimony_cost, `0`, `1`, Languages = ntips) %>% 
  xtable(digits = 0, caption = c("Most unstable features, Parsimony Gray et al 2009-tree."), label = c("unstable_parsimony_gray")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/parsimony_gray_all_unstable.txt")

parsimony_gray_top_stable_5_percent <- parsimony_gray %>% 
  arrange(Parsimony_cost) %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent = min / (`0`+ `1`)) %>%
  filter(min_percent > 0.05) %>% 
  .[1:5,] %>% 
  mutate(parsimony_gray_top_stable_5_percent = "Top-5")

parsimony_gray_top_stable_5_percent %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Parsimony cost` = Parsimony_cost, `0`, `1`, Languages = ntips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Parsimony Gray et al 2009-tree, only features with at least a distribution of 95% / 5%."), label = c("stable_parsimony_gray_5_precent")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/parsimony_gray_5_percent_stable.txt")



#######Maximum Likelihood####

##glottolog
ML_glottolog <- value_counts_ML_glottolog %>% 
  filter(nTips > ntips_half_glottolog) %>% 
  rename(loss = q10, gain = q01, `0` = nTips_state_0, `1` = nTips_state_1) %>% 
  inner_join(GB_df_desc, by = "Feature_ID") %>% 
  mutate(`Average rate` = (loss + gain)/2) %>% 
  mutate(`Average rate` = round(`Average rate`, 2)) %>% 
  mutate(`gain` = round(`gain`, 2)) %>% 
 mutate(`loss` = round(`loss`, 2))

ML_glottolog_top_stable <-  ML_glottolog %>% 
  arrange(`Average rate`) %>% 
  .[1:5,] %>% 
  mutate(ML_glottolog_top_stable = "Top-5")
  
  ML_glottolog_top_stable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Average rate`, gain, loss, `0`, `1`, Languages = nTips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Maximum Likelihood Glottolog-tree."), label = c("stable_ML_glottolog")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/ML_glottolog_top_stable.txt")

ML_glottolog_top_stable_5_percent <- ML_glottolog %>% 
  arrange(`Average rate`) %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent = min / (`0`+ `1`)) %>%
  filter(min_percent > 0.05) %>% 
  .[1:5,] %>%
  mutate(ML_glottolog_top_stable_5_percent = "Top-5")

ML_glottolog_top_stable_5_percent  %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Average rate`, gain, loss, `0`, `1`, Languages = nTips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Maximum Likelihood Glottolog-tree. Only features with at least a distribution of 95% / 5%."), label = c("stable_ML_glottolog_5")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/ML_glottolog_top_stable_5_percent.txt")

ML_glottolog_unstable <- ML_glottolog %>% 
  arrange(-`Average rate`) %>% 
  .[1:5,] %>% 
  arrange(`Average rate`) %>% 
  mutate(ML_glottolog_unstable = "Top-5")
  
ML_glottolog_unstable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Average rate`, gain, loss, `0`, `1`, Languages = nTips) %>% 
  xtable(digits = 0, caption = c("Most unstable features, Maximum Likelihood Glottolog-tree."), label = c("unstable_ML_glottolog")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/ML_glottolog_all_unstable.txt")

##Gray
ML_gray <- value_counts_ML_gray %>% 
  filter(nTips > ntips_half_gray) %>% 
  rename(loss = q10, gain = q01, `0` = nTips_state_0, `1` = nTips_state_1) %>% 
  inner_join(GB_df_desc, by = "Feature_ID") %>% 
  mutate(`Average rate` = (loss + gain)/2) %>% 
  mutate(`Average rate` = round(`Average rate`, 2)) %>% 
  mutate(`gain` = round(`gain`, 2)) %>% 
  mutate(`loss` = round(`loss`, 2))

ML_gray_top_stable <- ML_gray %>% 
  arrange(`Average rate`) %>% 
  .[1:5,] %>% 
  mutate(ML_gray_top_stable = "Top-5")
  
ML_gray_top_stable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Average rate`, gain, loss, `0`, `1`, Languages = nTips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Maximum Likelihood Gray et al 2009-tree."), label = c("stable_ML_gray")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/ML_gray_top_stable.txt")


ML_gray_top_stable_5_precent <- ML_gray %>% 
  arrange(`Average rate`) %>% 
  mutate(min = pmin(`0`, `1`)) %>% 
  mutate(min_percent = min / (`0`+ `1`)) %>%
  filter(min_percent > 0.05) %>% 
  .[1:5,] %>% 
  mutate(ML_gray_top_stable_5_precent = "Top-5")
  
  ML_gray_top_stable_5_precent %>%   
  dplyr::select(`Grambank ID` =  Abbreviation, `Average rate`, gain, loss, `0`, `1`, Languages = nTips) %>% 
  xtable(digits = 0, caption = c("Most stable features, Maximum Likelihood Gray et al 2009-tree. Only features with at least a distribution of 95% / 5%."), label = c("stable_ML_gray_5")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/ML_gray_top_stable_5_percent.txt")

ML_gray_top_unstable <- ML_gray %>% 
  arrange(-`Average rate`) %>% 
  .[1:5,] %>% 
  arrange(`Average rate`) %>% 
  mutate(ML_gray_top_unstable = "Top-5")
  
ML_gray_top_unstable %>% 
  dplyr::select(`Grambank ID` =  Abbreviation, `Average rate`, gain, loss, `0`, `1`, Languages = nTips) %>% 
  xtable(digits = 0, caption = c("Most unstable features, Maximum Likelihood Gray et al 2009-tree."), label = c("unstable_ML_gray")) %>% 
  print.xtable(include.rownames = F) %>% 
  write_lines("output/tree_tips_counts_per_feat_and_model/top_stable_latex_tables/ML_gray_all_unstable.txt")


all_unstable <- ML_gray_top_unstable %>% 
  mutate(ML_Gray_top_unstable = "yes") %>% 
  dplyr::select(Feature_ID, ML_Gray_top_unstable) %>% 
  full_join(ML_glottolog_unstable %>% 
              mutate(ML_glottolog_top_unstable = "yes") %>% 
            dplyr::select(Feature_ID, ML_glottolog_top_unstable))   %>% 
  full_join(parsimony_gray_unstable %>%  
              mutate(parsimony_Gray_top_unstable = "yes") %>% 
              dplyr::select(Feature_ID, parsimony_Gray_top_unstable)) %>% 

  full_join(parsimony_glottolog_top_unstable %>%  
              mutate(parsimony_Glottolog_top_unstable = "yes")
            %>% 
              dplyr::select(Feature_ID, parsimony_Glottolog_top_unstable)) 
  
  
All_stable <- ML_gray_top_stable_5_precent %>% 
  mutate(ML_Gray_top_stable = "Top-5") %>% 
  dplyr::select(Feature_ID, ML_Gray_top_stable,  ML_gray_rate = "Average rate") %>% 
  full_join(ML_glottolog_top_stable_5_percent %>%  
              mutate(ML_Glottolog_top_stable = "Top-5")
            %>% 
              dplyr::select(Feature_ID, ML_Glottolog_top_stable, ML_glottolog_rate = "Average rate")) %>% 
  full_join(parsimony_gray_top_stable_5_percent  %>%  
              mutate(parsimony_Glottolog_top_stable = "Top-5")
            %>% 
              dplyr::select(Feature_ID, parsimony_Glottolog_top_stable, parsimony_glottolog_cost = "Parsimony_cost")
            ) %>% 
  full_join(parsimony_gray_top_stable_5_percent %>%  
              mutate(parsimony_Gray_top_stable = "Top-5")
            %>% 
              dplyr::select(Feature_ID, parsimony_Gray_top_stable, parsimony_gray_cost = "Parsimony_cost")
            )  %>% 
  left_join(GB_df_desc) %>% 
  dplyr::select(Feature_ID, Abbreviation,parsimony_Glottolog_top_stable,	ML_Glottolog_top_stable,		parsimony_Gray_top_stable,  ML_Gray_top_stable, everything()
)

All_stable %>% 
  write_tsv("output/tree_tips_counts_per_feat_and_model/all_top_5_stable.tsv", na = "")