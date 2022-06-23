source("01_requirements.R")

#reading in old sheet with HL-predictions
#the reason for reading them in like this instead of subsetting the GB_wide table is because I'd like to use the LaTeX source formatting which exists in an extra col in the raw sheets

HL_findings_sheet <- read_tsv("../grambank-analysed/grambank/raw/Grambank/original_sheets/HS_cent2060.tsv", col_types = cols(.default = "c")) %>% 
  mutate("Proto-language" = "Proto-Central Pacific",
         Language_ID = "cent2060") %>% 
  filter(Feature_ID != "GB409") %>% 
  full_join(read_tsv("../grambank-analysed/grambank/raw/Grambank/original_sheets/HS_east2449.tsv", col_types = cols(.default = "c")) %>% 
              mutate("Proto-language" = "Proto-Eastern Polynesian",
                     Language_ID =  "east2449"),
            by = c("Feature_ID", "Feature", "Value", "Source (Latex)", "Source", "Comment", "Proto-language", "Language_ID")) %>% 
  full_join(read_tsv("../grambank-analysed/grambank/raw/Grambank/original_sheets/HS_poly1242.tsv", col_types = cols(.default = "c")) %>% 
              mutate("Proto-language" = "Proto-Polynesian",
                     Language_ID = "poly1242"),
            by = c("Feature_ID", "Feature", "Value", "Source (Latex)", "Source", "Comment", "Proto-language", "Language_ID")  ) %>% 
  full_join(read_tsv("../grambank-analysed/grambank/raw/Grambank/original_sheets/HS_ocea1241.tsv", col_types = cols(.default = "c"))%>% 
              mutate("Proto-language" = "Proto-Oceanic",
                     Language_ID =   "ocea1241"),
            by = c("Feature_ID", "Feature", "Value", "Source (Latex)", "Source", "Comment", "Proto-language", "Language_ID")) %>% 
  dplyr::select(Feature_ID, "Proto-language", Language_ID, Prediction = Value, "Historical Linguistics sources" = "Source (Latex)") 

HL_findings_sheet_wide <- HL_findings_sheet %>% 
  reshape2::dcast(`Proto-language` ~ Feature_ID, value.var = "Prediction") 

###Make coding sheets binarised

#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
if("GB024" %in% colnames(HL_findings_sheet_wide)){
  HL_findings_sheet_wide$GB024a <- ifelse(HL_findings_sheet_wide$GB024 == "1"|HL_findings_sheet_wide$GB024 == "3", "1", ifelse(HL_findings_sheet_wide$GB024 == "2", "0", NA)) 
  
  HL_findings_sheet_wide$GB024b <- ifelse(HL_findings_sheet_wide$GB024 == "2"|HL_findings_sheet_wide$GB024 == "3", "1", ifelse(HL_findings_sheet_wide$GB024 == "1", "0", NA)) 
}

#GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
if("GB025" %in% colnames(HL_findings_sheet_wide)){
  HL_findings_sheet_wide$GB025a <- ifelse(HL_findings_sheet_wide$GB025 == "1"|HL_findings_sheet_wide$GB025 == "3", "1", ifelse(HL_findings_sheet_wide$GB025 == "2", "0", NA)) 
  
  HL_findings_sheet_wide$GB025b <- ifelse(HL_findings_sheet_wide$GB025 == "2"|HL_findings_sheet_wide$GB025 == "3", "1", ifelse(HL_findings_sheet_wide$GB025 == "1", "0", NA)) 
}

#GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
if("GB065" %in% colnames(HL_findings_sheet_wide)){
  HL_findings_sheet_wide$GB065a <- ifelse(HL_findings_sheet_wide$GB065 == "1"|HL_findings_sheet_wide$GB065 == "3", "1", ifelse(HL_findings_sheet_wide$GB065 == "2", "0", NA)) 
  
  HL_findings_sheet_wide$GB065b <- ifelse(HL_findings_sheet_wide$GB065 == "2"|HL_findings_sheet_wide$GB065 == "3", "1", ifelse(HL_findings_sheet_wide$GB065 == "1", "0", NA)) 
}

#GB130 multistate 1: SV; 2: VS; 3: both
if("GB130" %in% colnames(HL_findings_sheet_wide)){
  HL_findings_sheet_wide$GB130a <- ifelse(HL_findings_sheet_wide$GB130 == "1"|HL_findings_sheet_wide$GB130 == "3", "1", ifelse(HL_findings_sheet_wide$GB130 == "2", "0", NA)) 
  
  HL_findings_sheet_wide$GB130b <- ifelse(HL_findings_sheet_wide$GB130 == "2"|HL_findings_sheet_wide$GB130 == "3", "1", ifelse(HL_findings_sheet_wide$GB130 == "1", "0", NA)) 
}

#GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
if("GB193" %in% colnames(HL_findings_sheet_wide)){
  HL_findings_sheet_wide$GB193a <- ifelse(HL_findings_sheet_wide$GB193 == "1"|HL_findings_sheet_wide$GB193 == "3", "1", ifelse(HL_findings_sheet_wide$GB193 == "2"|HL_findings_sheet_wide$GB193 == "0", "0", NA)) 
  
  HL_findings_sheet_wide$GB193b <- ifelse(HL_findings_sheet_wide$GB193 == "2"|HL_findings_sheet_wide$GB193 == "3", "1", ifelse(HL_findings_sheet_wide$GB193 == "1"|HL_findings_sheet_wide$GB193 == "0", "0", NA)) 
}
#GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.
if("GB203" %in% colnames(HL_findings_sheet_wide)){
  HL_findings_sheet_wide$GB203a <- ifelse(HL_findings_sheet_wide$GB203 == "1"|HL_findings_sheet_wide$GB203 == "3", "1", ifelse(HL_findings_sheet_wide$GB203 == "2"|HL_findings_sheet_wide$GB203 == "0", "0", NA)) 
  
  HL_findings_sheet_wide$GB203b <- ifelse(HL_findings_sheet_wide$GB203 == "2"|HL_findings_sheet_wide$GB203 == "3", "1", ifelse(HL_findings_sheet_wide$GB203 == "1"|HL_findings_sheet_wide$GB203 == "0", "0", NA)) 
}

GB_df_desc <-  data.table::fread("data/GB/parameters_binary.tsv" ,
                                 encoding = 'UTF-8', 
                                 quote = "\"", 
                                 fill = T, 
                                 header = TRUE, 
                                 sep = "\t") %>% 
  filter(Binary_Multistate != "Multi") %>% #we are only interested in the binary or binarised features.
  dplyr::select(Feature_ID = ID)

HL_findings_sheet_wide %>% 
  reshape2::melt(id.vars = "Proto-language") %>% 
  rename(Feature_ID = variable, Prediction = value) %>% 
  filter(!is.na(Prediction)) %>% 
  inner_join(GB_df_desc, by = "Feature_ID") %>% #filter out multivalue features
   write_tsv("data/HL_findings/HL_findings_for_comparison.tsv")

####

##reading in old sheet with HL-predictions - ergative section
#HL_findings_sheet_conflict <- read_csv("data/HL_findings/HL_findings_conflicts.csv")%>%   
#  dplyr::select(Feature_ID, "Proto-language", Language_ID, Prediction = Value, "Historical Linguistics sources" = "Source (Latex)") 

  