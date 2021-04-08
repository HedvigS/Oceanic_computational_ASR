#This script takes the values and languages tables from a cldf-release and combines then and transforms them to a wide data format from a long. It does not take into account the parameter or code tables.

if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") } #if pacman isn't already installed, install it.

pacman::p_load(
  dplyr,#for data wrangling
  jsonlite, #reading json files
  stringr, #for string evaluation
  readr #for reading in data files
)

#finding the filenames for the two tables we are interested in, the language and value tables. The specific filenames can vary, so instead of identifying them via the filename we should check which of the tables conform to particular CLDF-standards and then take the filenames for the tables that conform to those standards fromt the meta-datajson.

#since grambank-cldf isn't public yet, I'm running things from the local machine clone in the meantime

cldf_github_folder <- "../../grambank-cldf/cldf"
#cldf_github_folder <-"https://github.com/glottobank/grambank-cldf/tree/master/cldf"

file_index_json <- list.files(cldf_github_folder) %>% grep(pattern = "json")

fn_json <- list.files(cldf_github_folder)[file_index_json]

cldf_json <- jsonlite::read_json(file.path(cldf_github_folder, fn_json))


#creating a folder for outputting tables
if (!dir.exists("data")) { dir.create("data") }

#finding the fileanme for the relevant tables by checking which of the tables entered into the json meta data file conforms to a given cldf-standard and pulling the filename from there

#the below for loop works for the glottolog-cldf set, but does not  work for grambank-cldf. This is because the second table does not have the dc:conformsTo value. It seems like Robert is changing that though, so in the meantime we'll just do something cruder until we can switch back to the for-loop method
#example with glottolog-cldf: https://github.com/HedvigS/personal-cldf-cookbook/blob/main/R/make_lang_values_wide_fetch_online.R#L41

####
 index <- 0

 #going over each table in the json and checking which one conforms and saving the index of that one to a separate variable
 for (table in cldf_json$tables ) {
   
  index <- index +1

   if("dc:conformsTo" %in% names(table) & !is.null(table$`dc:conformsTo`)) { 
          if(table$`dc:conformsTo` == "http://cldf.clld.org/v1.0/terms.rdf#ValueTable") {index_ValueTable  <- index}
 }}
 

# #using the index we derived above, pulling out the filename for that table
 values_fn_name <- cldf_json$tables[index_ValueTable][[1]]$url #not sure why this has the name "url" when it is just the filename but that is the way
 values_csv_fn <- file.path(cldf_github_folder, values_fn_name) #creating the file path
####


#reading in data and making it wide
values <- readr::read_csv(values_csv_fn, na = c("","<NA>"), col_types = cols()) %>% 
  reshape2::dcast(Language_ID ~ Parameter_ID, value.var = "Value") #making long data wide

write_tsv(values, "data/GB_wide.tsv")