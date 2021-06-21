#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

source("1_requirements.R")

#finding the filenames for the two tables we are interested in, the language and value tables. The specific filenames can vary, so instead of identifying them via the filename we should check which of the tables conform to particular CLDF-standards and then take the filenames for the tables that conform to those standards fromt the meta-datajson.

#since grambank-cldf isn't public yet, I'm running things from the local machine clone in the meantime

config_json <- jsonlite::read_json("config.json")

grambank_cldf_github_folder <- config_json$data_sources$grambank_cldf$location

file_index_json <- list.files(grambank_cldf_github_folder) %>% grep(pattern = "json")

fn_json <- list.files(grambank_cldf_github_folder)[file_index_json]

grambank_cldf_json <- jsonlite::read_json(file.path(grambank_cldf_github_folder, fn_json))


#creating a folder for outputting tables
if (!dir.exists("data")) { dir.create("data") }

#finding the fileanme for the relevant tables by checking which of the tables entered into the json meta data file conforms to a given cldf-standard and pulling the filename from there

#the below for loop works for the glottolog-cldf set, but does not  work for grambank-cldf. This is because the second table does not have the dc:conformsTo value. It seems like Robert is changing that though, so in the meantime we'll just do something cruder until we can switch back to the for-loop method
#example with glottolog-cldf: https://github.com/HedvigS/personal-cldf-cookbook/blob/main/R/make_lang_values_wide_fetch_online.R#L41

####
 index <- 0

 #going over each table in the json and checking which one conforms and saving the index of that one to a separate variable
 for (table in grambank_cldf_json$tables ) {
   
  index <- index +1

   if("dc:conformsTo" %in% names(table) & !is.null(table$`dc:conformsTo`)) { 
          if(table$`dc:conformsTo` == "http://cldf.clld.org/v1.0/terms.rdf#ValueTable") {index_ValueTable  <- index}
 }}
 

# #using the index we derived above, pulling out the filename for that table
 values_fn_name <- grambank_cldf_json$tables[index_ValueTable][[1]]$url #not sure why this has the name "url" when it is just the filename but that is the way
 values_csv_fn <- file.path(grambank_cldf_github_folder, values_fn_name) #creating the file path
####


#reading in data and making it wide
values <- readr::read_csv(values_csv_fn, na = c("","<NA>"), col_types = cols()) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") #making long data wide



#reading in the parameters table

####
index <- 0

#going over each table in the json and checking which one conforms and saving the index of that one to a separate variable
for (table in grambank_cldf_json$tables ) {
  
  index <- index +1
  
  if("dc:conformsTo" %in% names(table) & !is.null(table$`dc:conformsTo`)) { 
    if(table$`dc:conformsTo` == "http://cldf.clld.org/v1.0/terms.rdf#CodeTable") {index_CodeTable  <- index}
  }}

codes_fn_name <- grambank_cldf_json$tables[index_CodeTable][[1]]$url #not sure why this has the name "url" when it is just the filename but that is the way
codes_csv_fn <- file.path(grambank_cldf_github_folder, codes_fn_name ) #creating the file path

codes_df <- readr::read_csv(codes_csv_fn , na = c("","<NA>"), col_types = cols()) 


#binarising it 
#GB contains a small set of multistate features. They can be binarised, but they need to be done so in a particular way. This code renders a appropriately binarised version of the dataset.

multistate_features <- codes_df  %>% 
  unite(col = possible_values_split, Name,  Description, sep = ": ") %>% 
  group_by(Parameter_ID) %>% 
  mutate(possible_values = paste0(possible_values_split, collapse = ", ")) %>% 
  distinct(Parameter_ID, possible_values) %>% 
  filter(str_detect(possible_values, "2")) %>% 
  dplyr::select(Parameter_ID) %>% 
  as.matrix() %>% 
  as.vector()

values_multi <- values %>% 
  column_to_rownames("Language_ID") %>% 
  dplyr::select(contains(multistate_features))



#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
if("GB024" %in% colnames(values_multi)){
  values_multi$GB024a <- if_else(values_multi$GB024 == "1"|values_multi$GB024 == "3", "1", ifelse(values_multi$GB024 == "2", "0", NA)) 
  
  values_multi$GB024b <- if_else(values_multi$GB024 == "2"|values_multi$GB024 == "3", "1", ifelse(values_multi$GB024 == "1", "0", NA)) 
}

#GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
if("GB025" %in% colnames(values_multi)){
  values_multi$GB025a <- if_else(values_multi$GB025 == "1"|values_multi$GB025 == "3", "1", ifelse(values_multi$GB025 == "2", "0", NA)) 
  
  values_multi$GB025b <- ifelse(values_multi$GB025 == "2"|values_multi$GB025 == "3", "1", ifelse(values_multi$GB025 == "1", "0", NA)) 
}

#GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
if("GB065" %in% colnames(values_multi)){
  values_multi$GB065a <- if_else(values_multi$GB065 == "1"|values_multi$GB065 == "3", "1", ifelse(values_multi$GB065 == "2", "0", NA)) 
  
  values_multi$GB065b <- if_else(values_multi$GB065 == "2"|values_multi$GB065 == "3", "1", ifelse(values_multi$GB065 == "1", "0", NA)) 
}

#GB130 multistate 1: SV; 2: VS; 3: both
if("GB130" %in% colnames(values_multi)){
  values_multi$GB130a <- if_else(values_multi$GB130 == "1"|values_multi$GB130 == "3", "1", ifelse(values_multi$GB130 == "2", "0", NA)) 
  
  values_multi$GB130b <- if_else(values_multi$GB130 == "2"|values_multi$GB130 == "3", "1", ifelse(values_multi$GB130 == "1", "0", NA)) 
}

#GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
if("GB193" %in% colnames(values_multi)){
  values_multi$GB193a <- if_else(values_multi$GB193 == "1"|values_multi$GB193 == "3", "1", ifelse(values_multi$GB193 == "2"|values_multi$GB193 == "0", "0", NA)) 
  
  values_multi$GB193b <- if_else(values_multi$GB193 == "2"|values_multi$GB193 == "3", "1", ifelse(values_multi$GB193 == "1"|values_multi$GB193 == "0", "0", NA)) 
}
#GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.
if("GB203" %in% colnames(values_multi)){
  values_multi$GB203a <- if_else(values_multi$GB203 == "1"|values_multi$GB203 == "3", "1", ifelse(values_multi$GB203 == "2"|values_multi$GB203 == "0", "0", NA)) 
  
  values_multi$GB203b <- if_else(values_multi$GB203 == "2"|values_multi$GB203 == "3", "1", ifelse(values_multi$GB203 == "1"|values_multi$GB203 == "0", "0", NA)) 
}

values_multi_only_binarized <- values_multi %>% 
  dplyr::select(-all_of(multistate_features)) %>% 
  rownames_to_column("Language_ID")

stopifnot(all(!multistate_features %in% colnames(values_multi_only_binarized)))

output_path <- file.path("data", "GB", "GB_wide_binarised.tsv")

GB_wide <- values %>% 
  dplyr::select(-all_of(multistate_features)) %>% 
  full_join(values_multi_only_binarized, by = "Language_ID") %>% 
  dplyr::select(Language_ID, everything())  # reordering columns for inspection convenience

##AGGREGATE dialects to langauge
# In order for the rescaling of the branches to an ultrametric tree to make sense, we should aggregate all dialects to language level. Otherwise, we'd have dialects as tips alongside languages.

#reading in glottolog language table (to be used for aggregating to Language_level_ID)
glottolog_df <- read_tsv("data/glottolog_language_table_wide_df.tsv", col_types = cols())  %>% 
  dplyr::select(Language_ID= Glottocode, Language_level_ID, level)

#One of the reasons this makes sense to do is because there are no dialect to dialect matches between the Grambank data and the gray et al 2009-tree. Let's just double check that though in case you added new coding. If there is a match, we should do things differently. The code flow to the next scripts will break if there is a match.
config_json <- jsonlite::read_json("config.json")

dplace_github_repos_fn <- config_json$data_sources$d_place_gray_et_al_2009_tree$location

Gray_et_al_tree_taxon_fn <- paste0(dplace_github_repos_fn, "/phylogenies/gray_et_al2009/taxa.csv")
taxa <- read_csv(Gray_et_al_tree_taxon_fn, col_types = cols()) %>% 
  rename(Language_ID = glottocode) #to conform to what glottolog does elsewhere

dialect_matches_gray_et_al_tree_grambank <- GB_wide %>% 
  dplyr::select(Language_ID) %>% 
  left_join(glottolog_df, by = "Language_ID") %>% 
  filter(level == "dialect") %>% 
  inner_join(taxa, by = "Language_ID") %>% nrow()

if(dialect_matches_gray_et_al_tree_grambank == 0){
  cat("There were no matches of dialect to dialect between the Gray et al 2009-tree and Grambank, therefore it makes sense to generally aggregate to language-level for both trees. \n")

GB_wide %>% 
  reshape2::melt(id.vars= "Language_ID") %>%
  filter(value == "1" | value == "0") %>% #we want to pick a non-? value, so let's just filter to only 0's and 1's
  left_join(glottolog_df, by = "Language_ID") %>%
  dplyr::select(Language_ID, Language_level_ID, everything()) %>% 
  group_by(Language_level_ID, variable) %>% 
  sample_n(1) %>% #for rows where dialects of the same language are coded for the same feature, pick a value at random from the available one 
  reshape2::dcast(Language_level_ID ~ variable, value.var = "value") %>%
  rename(Language_ID = Language_level_ID) %>% 
  write_tsv(output_path)

################################


#adapting the parameters file for the binarised features


####
index <- 0

#going over each table in the json and checking which one conforms and saving the index of that one to a separate variable
for (table in grambank_cldf_json$tables ) {
  
  index <- index +1
  
  if("dc:conformsTo" %in% names(table) & !is.null(table$`dc:conformsTo`)) { 
    if(table$`dc:conformsTo` == "http://cldf.clld.org/v1.0/terms.rdf#ParameterTable") {index_CodeTable  <- index}
  }}

parameters_fn_name <- grambank_cldf_json$tables[index_CodeTable][[1]]$url #not sure why this has the name "url" when it is just the filename but that is the way
parameters_csv_fn <- file.path(grambank_cldf_github_folder, parameters_fn_name ) #creating the file path

parameters_df <- readr::read_csv(parameters_csv_fn, na = c("","<NA>"), col_types = cols()) 

Parameter_desc_binary <- tibble(
  ID = c(
    "GB024a", "GB024b",
    "GB025a", "GB025b",
    "GB065a", "GB065b",
    "GB130a","GB130b",
    "GB193a","GB193b",
    "GB203a", "GB203b"
  ),
  Grambank_ID_desc = c(
    "GB024a NUMOrder_Num-N",
    "GB024b NUMOrder_N-Num",  
    "GB025a DEMOrder_Dem-N",
    "GB025b DEMOrder_N-Dem",
    "GB065a POSSOrder_PSR-PSD",
    "GB065b POSSOrder_PSD-PSR",
    "GB130a IntransOrder_SV",
    "GB130b IntransOrder_VS",
    "GB193a ANMOrder_ANM-N",
    "GB193b ANMOrder_N-ANM",
    "GB203a UQOrder_UQ-N",
    "GB203b UQOrder_N-UQ"
  ),
  `OV vs VO types (excl affixes)`= c(
    "OV",
    "VO",
    "OV",
    "VO",
    "OV",
    "VO",
    NA,
    NA,
    "OV",
    "VO",
    "OV",
    "VO"
  ),
  `OV VO score for counting`= c(
    0,
    1,
    0,
    1,
    0,
    1,
    NA,
    NA,
    0,
    1,
    0,
    1
  ), 
  Binary_Multistate = c("binarised","binarised","binarised","binarised","binarised","binarised","binarised","binarised","binarised","binarised","binarised","binarised")
) %>% full_join(parameters_df, by = c("ID", "Grambank_ID_desc"))


Parameter_desc_binary %>% 
  mutate(Binary_Multistate= ifelse(ID %in% multistate_features, "Multi", Binary_Multistate)) %>% 
  mutate(Binary_Multistate = ifelse(is.na(Binary_Multistate), "Binary", Binary_Multistate)) %>% 
  write_tsv(file.path("data", "GB", "parameters_binary.tsv"))

cat("Grambank made wide and binarised. Wide table and parameters table written.\n")

} else{message("Stop the presses!!! There was a match of dialect to dialect between the Gray et al 2009-tree and Grambank! Let's have a rethink!")}