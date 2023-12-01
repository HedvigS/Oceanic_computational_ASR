source("01_requirements.R")

#this script takes a tex file and filters to all the \cite commands 

tex <- readLines("../tex/ASR_Oceanic.tex") %>% 
  as.data.frame() %>% 
  rename(text = ".") %>% 
  mutate(text = str_replace(text, pattern = "\\\\%", replacement = "\\€")) %>% #replace "actual" percentage marks with something else in order to use the comment percent marks 
  mutate(text = str_replace(text, pattern = "%.*",replacement =  ""))  %>% #replace any content that starts with comment out percent mark with nothing, i.e. remove them
  mutate(text = str_replace(text, pattern = "\\€", replacement = "\\\\%")) %>% #put the actual percent marks back in
  mutate(text = str_split(text, " ")) %>% #split at spaces
    unnest(text) %>% #unnest, i.e. each "word" (thing between spaces) on one line
  filter(str_detect(text, "\\\\cite")) %>% #filter to only those with the cite command
  mutate(text = str_extract(text, "\\\\cite.*\\}")) %>% #extract so that irrelevant things are dropped
  mutate(text = str_replace(text, "\\[.*\\]", "")) %>% #remove page number arguments
  mutate(text = str_replace(text, "\\\\cite.*\\{", "")) %>% #remove cite command
  mutate(text = str_replace(text, "\\}", "")) %>% #remove last curly bracket
  mutate(text = str_replace(text, "\\}.*", "")) #remove last curly bracket

cat(paste0("You used ", tex %>% nrow(), " citations.\n"))
cat(paste0("You cited ", distinct(tex) %>% nrow(), " unique papers.\n"))

cat(paste0("Your top five most cited references were: \n"))

print(
table(tex$text) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1:5,] %>% 
  as.matrix())

tex %>% 
  distinct(text) %>% 
  write_tsv("output/processed_data/tex_refs.tsv")
