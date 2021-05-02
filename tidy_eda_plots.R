#load data and libs
library(tidyverse)
library(gridExtra)
library(grid)

Traits.clean <- read.csv("derived_data/Clean.Traits.csv")
Coupling <- read.csv("derived_data/Druguser.cp.csv")

#how about histograms overlayed split by who is  THC , alcohol dependent and who is not

#first join coupling with traits
DF <- left_join(Coupling, Traits.clean, by = "Subject")

#check some of the drug traits 
# nrow(DF[which(DF$Cocaine == "true"), ]) #only 5 coke users
# nrow(DF[which(DF$Opiates == "true"), ]) #onoly 7 opiate users
# nrow(DF[which(DF$MethAmphetamine == "true"), ]) #only 5
# nrow(DF[which(DF$Oxycontin == "true"), ]) #only 4 of these
# nrow(DF[which(DF$SSAGA_Mj_Ab_Dep == 1), ]) #99 constant weed users
# nrow(DF[which(DF$SSAGA_Alc_D4_Dp_Dx == 5), ]) #61 Alc dependent

#lets do THC dependence
p1 <- DF %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(SSAGA_Mj_Ab_Dep)), alpha = .5) +
  scale_fill_discrete("Marijuana Dependence") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Marijuana Dependence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#lets do Alc dependence
p2 <- DF %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(SSAGA_Alc_D4_Dp_Dx)), alpha = .5) +
  scale_fill_discrete("Alcohol Dependence") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Alcohol Dependence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#alc abuse
p3 <- DF %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(SSAGA_Alc_D4_Ab_Dx)), alpha = .5) +
  scale_fill_discrete("Alcohol Abuse") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Alcohol Abuse") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#lets do all other hard drugs combined since their aren't many in each
p4 <- DF %>% 
  mutate(Hard_drugs = ifelse(Cocaine == "true" | MethAmphetamine == "true" |
                               Oxycontin == "true" | Opiates == "true" |
                               Amphetamines == "true", 1, 0)) %>% 
  select(Cocaine, MethAmphetamine, Oxycontin, Opiates, Amphetamines, Hard_drugs, coupling) %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(Hard_drugs)), alpha = .5) +
  scale_fill_discrete("Hard Drug Use") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Hard Drug Use", 
       caption = "Data Source: https://www.humanconnectome.org") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

title <- textGrob("FC and SC Coupling Factor", gp = gpar(fontface = "bold", cex = 1.5))
plot1 <- grid.arrange(p1, p2, p3, p4, nrow = 2, 
                      top = title)

ggsave("prelim_graphics/Drug.Histograms.png", plot1)