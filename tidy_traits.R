#load data and libs
library(tidyverse)
library(readxl)

#ok lets bring in all the traits for the 1206 people

traits.table1 <- read.csv("Source_Data/traits/table1_hcp.csv")
traits.table2 <- read.csv("Source_Data/traits/table2_hcp.csv")

#get the dictionary as well and we can choose traits from this using the category
trait.dict <- read_excel("Source_Data/traits/HCP_S1200_DataDictionary_Sept_18_2017.xls")

#we can use the dict to find all the interesting column headers so we know what we want

#ok lets start with the substance abuse

substance <- trait.dict %>% 
  filter(category == "Substance Use") %>% 
  select(fullDisplayName, assessment, columnHeader, description)

#the interesting traits I want to look at here are mostly for drug use

#I want all the drug test results
#the column headers are as follows: Cocaine, THC, Opiates, 
#Amphetamines, MethAmphetamine, Oxycontin

#I think I only want he binary results and maybe a few others, 
#so for alcohol we want the binary for if they are dependent or abusing it
# Participant meets DSM4 criteria for Alcohol Abuse: SSAGA_Alc_D4_Ab_Dx
# Participant meets DSM4 criteria for DSM4 Alcohol Dependence: SSAGA_Alc_D4_Dp_Dx

#I don't think looking at tobacco is as interesting as harder drugs, so lets skip that

#marijuana
# Ever used marijuana: no = 0; yes = 1: SSAGA_Mj_Use
# Participant meets DSM criteria for Marijuana Dependence: SSAGA_Mj_Ab_Dep

#ok lets save that set
substance.interest <- substance %>% 
  filter(columnHeader == "Cocaine" | columnHeader == "THC" | columnHeader == "Opiates" |
           columnHeader == "Amphetamines" | columnHeader == "MethAmphetamine" |
           columnHeader == "Oxycontin" | columnHeader == "SSAGA_Alc_D4_Ab_Dx" |
           columnHeader == "SSAGA_Alc_D4_Dp_Dx" | columnHeader == "SSAGA_Mj_Use" |
           columnHeader == "SSAGA_Mj_Ab_Dep")
column.header.list.substance <- substance.interest$columnHeader


#also we want emotions

emotions <- trait.dict %>% 
  filter(category == "Emotion") %>% 
  select(fullDisplayName, assessment, columnHeader, description)
column.header.list.emotions <- emotions$columnHeader

#im not sure exactly what I want for the emotions so we can just keep them all for now

#ok now I need to find out what traits are in what table and take them
# table1.names <- names(traits.table1)[names(traits.table1) %in% column.header.list.substance]
#ok they arent in table 1, lets check table 2
table2.names <- names(traits.table2)[names(traits.table2) %in% column.header.list.substance]
#ok so all the substance abuse ones are in table 2 we can now subset table2 
#to only have these cols and the id
table2.interest <- traits.table2[, c("Subject", table2.names)]
#unique(table2.interest$SSAGA_Alc_D4_Ab_Dx) i think 1 is not dependent and 5 is dependent

#now do a similar thing for the emotions
table1.names <- names(traits.table1)[names(traits.table1) %in% column.header.list.emotions]
#ok great these are all in table1
table1.interest <- traits.table1[, c("Subject", table1.names)]


#now to make a clean traits table we can cbind them on subject

clean.traits <- full_join(table1.interest, table2.interest)

#awesome now lets save this

write.csv(clean.traits, "derived_data/Clean.Traits.csv", row.names = F)