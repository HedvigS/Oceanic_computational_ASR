source("01_requirements.R")
source("fun_def_h_load.R")

h_load("NCmisc")
source("fun_def_list.functions.R")

dir <- "../tex/bib_from_r/"
if(!dir.exists(dir)){dir.create(dir)}

r_fns <- list.files(path = ".", pattern = "*.R$", full.names = T, recursive = T)

r_grambank <- c("../grambank-analysed/R_grambank/make_wide.R", "../grambank-analysed/R_grambank/make_wide_binarized.R", "../grambank-analysed/R_grambank/make_glottolog-cldf_table.R")

r_fns <- c(r_fns, r_grambank)

df <- data.frame("packages" = as.character(),
                 "functions" = as.character(),
                 "scripts" = as.character())


for(fn in r_fns){

#  fn <- r_fns[5]
  
  cat(paste0("I'm on ", fn, ".\n"))
  
x <- list.functions.in.file_tweaked(filename = fn, unique = F) %>% 
  as.data.frame() %>% 
  rownames_to_column("packages") %>% 
  rename("functions" = 2) %>% 
  mutate(packages = str_replace_all(packages, "package:", "")) %>% 
  mutate(packages = str_replace_all(packages, "c\\(", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\)", "")) %>% 
  mutate(packages = str_replace_all(packages, "\\\"", "")) %>% 
  mutate(packages = str_replace_all(packages, "character\\(0", "")) %>% 
  mutate(packages = str_split(packages, ",")) %>% 
  unnest(cols = "packages") %>% 
  unnest(cols = "functions") %>% 
  mutate(scripts = fn)

df <- full_join(x, df, by = c("packages", "functions", "scripts"))
}

used_packages <- df %>% 
  mutate(used = "TRUE")

# dealing with instances where a package wasn't found. in pipe above this was listed as "" but it should be a proper NA
used_packages <- naniar::replace_with_na(data = used_packages, replace= list(packages = ""))
used_packages$packages <- trimws(used_packages$packages)

#df with loaded packages
loaded_packages <- data.frame(packages = (.packages())) %>% 
  mutate(loaded = "TRUE")

joined_df <- full_join(used_packages, loaded_packages, by = "packages")

unused_but_loaded <- joined_df %>% 
  filter(is.na(used)) %>% 
  filter(!is.na(loaded)) 

cat("There are ", nrow(unused_but_loaded), "packages that it seems like you're not using, but that are loaded.\n They are: ", unused_but_loaded$packages, ".\n" )

most_used <- used_packages %>% 
  filter(packages != ".GlobalEnv") %>% 
  filter(!is.na(packages)) %>% 
  filter(!is.na(used)) %>% 
  group_by(packages) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 
  
cat("The top 5 packages from which you use the most functions are:\n ")
most_used[1:5,]

most_used$packages <- fct_reorder(most_used$packages, most_used$n)

p <- most_used %>% 
  ggplot() +
  geom_bar(aes(x = packages, y = n, fill = n), stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), 
        legend.position = 0) 

ggsave(plot = p, "output/processed_data/used_packages.png")

script_with_most_functions <-  used_packages %>% 
#  distinct(scripts, functions) %>% 
  group_by(scripts) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

cat("The top 5 scripts which use the most functions:\n ")
script_with_most_functions [1:5,]

#generating bibtex file of all the packages where you've used at least one funciton
h_load("knitr")

output_fn <- "../tex/bib_from_r/used_pkgs.bib"

extra_packages <- c("ggpubr", "cluster", "psych", "bib2df", "knitr", "remotes")

pkgs <- c(unique(as.character(most_used$packages)), extra_packages) %>% sort()

knitr::write_bib(pkgs, file = output_fn)

cat(paste0("Wrote citations for packages you've used to", output_fn, ".\n There were ", length(pkgs)
, " entries.\n" ))

#optional part, this generates a text string with the bibtex citation KEYS from earlier that you can then paste into LaTeX in order to cite all

h_load("bib2df")

bibdf <- suppressWarnings(suppressMessages(bib2df(output_fn)))

#solution to getting an "and" on the last line from SO
# https://stackoverflow.com/questions/42456452/changing-vector-to-string-special-separator-for-last-element
fPaste <- function(vec) sub(",\\s+([^,]+)$", " and \\1", toString(vec))

vec <- paste0("\\citet{", bibdf$BIBTEXKEY, "}")

fPaste(vec)   %>% 
  sort() %>% 
  writeLines(con = "../tex/bib_from_r/citation_keys.txt")
