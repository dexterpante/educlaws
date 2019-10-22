# educlaws
#Text analysis of education related laws.  This includes data cleaning and wrangling.

library(tidyverse)
library(readxl)
library(lubridate)
laws <- read_excel("DepEd Laws.xls",
col_types = c("text", "text", "date"))
laws$TITLE <- tolower(laws$TITLE)
#transforms text into lower case

minilaws <- laws %>% filter(str_detect(TITLE, "school")) %>% 
  filter(!str_detect(TITLE, "division"))
#filters only laws related to schools. there are some laws w/c provide for creation of division offices

minilaws$PROCEDURE <- NA #creates new var
minilaws$LEVEL <- NA #creates new var
minilaws <- minilaws %>% mutate (YEAR=year(ENACTMENT))%>% 
  mutate(LEVEL=ifelse(str_detect(TITLE, "elementary"), "elem",
                      ifelse(str_detect(TITLE, "senior"),"shs","jhs"))) %>%
  mutate(PROCEDURE =  ifelse(str_detect(TITLE, "esta"),"establishment", 
                      ifelse(str_detect(TITLE, "creat"), "establishment",
                      ifelse(str_detect(TITLE, "conver"), "separation/conversion", 
                      ifelse(str_detect(TITLE, "integ"), "integration",
                      ifelse(str_detect(TITLE, "renaming"),"rename",
                      ifelse(str_detect(TITLE, "separat"),"separation/conversion",
                      ifelse(str_detect(TITLE, "transfer"), "rename",
                      ifelse(str_detect(TITLE, "changing"),"rename","others"))))))))) 
#creates new vars

pattern <- "as\\s.+\\sschool"
minilaws$SCHNAME <- str_extract(minilaws$TITLE, pattern)
minilaws$SCHNAME <- str_replace(minilaws$SCHNAME, "as\\sthe\\s","")
minilaws$SCHNAME <- str_replace(minilaws$SCHNAME, "as\\s","")
#performs data cleaning

pattern2 <- c("municipality\\s.+\\sprovince|city\\sof\\s.+\\sprovince")
minilaws$MUNICIPALITY <- str_extract(minilaws$TITLE, pattern2)
minilaws$MUNICIPALITY <- str_replace(minilaws$MUNICIPALITY, "municipality\\sof\\s","")
minilaws$MUNICIPALITY <- str_replace(minilaws$MUNICIPALITY, ",\\s.+","")
minilaws$MUNICIPALITY <- str_replace(minilaws$MUNICIPALITY, "city\\sof\\s","")
#performs data cleaning

pattern3 <- "(province\\sof\\s.+) (,|to\\sbe)"
minilaws$PROVINCE <- str_extract(minilaws$TITLE, pattern3)
minilaws$PROVINCE <- str_replace(minilaws$PROVINCE, "province\\sof\\s","")
minilaws$PROVINCE <- str_replace(minilaws$PROVINCE, ",\\sto\\sbe","")
minilaws$PROVINCE <- str_replace(minilaws$PROVINCE, "\\sto\\sbe","")
minilaws$PROVINCE <- str_replace(minilaws$PROVINCE, "\\sfrom\\s.+|\\sinto\\s.+|\\sand\\s.+","")
minilaws$PROVINCE <- str_replace(minilaws$PROVINCE, ",","")
#performs data cleaning

minilaws$TITLE <- str_replace(minilaws$TITLE, "^\\s","")
minilaws$NUMBER <- str_replace(minilaws$NUMBER, "^\\s","")
#performs data cleaning

minilaws <- minilaws%>% select(SCHNAME, MUNICIPALITY, PROVINCE, LEVEL, YEAR, PROCEDURE, NUMBER, TITLE, ENACTMENT)
#reorders dataset

write.csv(minilaws, "minilaws.csv")

minilaws <- minilaws %>% select(SCHNAME, PROVINCE, LEVEL, YEAR, PROCEDURE)








