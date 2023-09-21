source("01_requirements.R")

tex <- readLines("../tex/ASR_Oceanic.tex") %>% 
  as.data.frame() %>% 
  rename(text = ".") %>% 
  mutate(text = str_replace(text, pattern = "\\\\%", replacement = "\\€")) %>% #remove commented out
  mutate(text = str_replace(text, pattern = "%.*",replacement =  ""))  %>% 
  mutate(text = str_replace(text, pattern = "\\€", replacement = "\\\\%")) %>%
  mutate(text = str_split(text, " ")) %>% 
    unnest(text) %>% 
  filter(str_detect(text, "\\\\cite")) %>%
  mutate(text = str_extract(text, "\\\\cite.*\\}")) %>% 
  mutate(text = str_replace(text, "\\[.*\\]", "")) %>% 
  mutate(text = str_replace(text, "\\\\cite.*\\{", "")) %>% 
  mutate(text = str_replace(text, "\\}", ""))

cat(paste0("You cited a paper ", tex %>% nrow(), " times.\n"))
cat(paste0("You cited ", distinct(tex) %>% nrow(), " unique papers.\n"))

cat(paste0("Your top five most cited references were: "))

print(
table(tex$text) %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1:5,] %>% 
  as.matrix())
