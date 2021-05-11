#load data and libs
library(tidyverse)
library(gridExtra)
library(grid)

DF <- read.csv("derived_data/model_data_TNPCA_FC.csv")

#how about histograms overlayed split by who is  THC , alcohol dependent and who is not

#lets do MJ
p1 <- DF %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(mari.user)), alpha = .5) +
  scale_fill_discrete("Marijuana use") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Marijuana USE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#lets do Alc dependence
p2 <- DF %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(Alc)), alpha = .5) +
  scale_fill_discrete("Alcohol Use") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Alcohol Use") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#lets do all other hard drugs combined since their aren't many in each
p3 <- DF %>% 
  ggplot(aes(x = coupling)) +
  geom_density(aes(fill = as.factor(hard.drug)), alpha = .5) +
  scale_fill_discrete("Hard Drug Use") +
  labs(x = "Coupling Factor", 
       y = "Density", 
       title = "Hard Drug Use", 
       caption = "Data Source: https://www.humanconnectome.org") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

title <- textGrob("FC and SC Coupling Factor & Substance Abuse", gp = gpar(fontface = "bold", cex = 1.5))
m <- matrix(c(1, 1, 2, 2, 3, 4, 4, 5), ncol = 4, byrow = T)
plot <- grid.arrange(p1, p2, grid::nullGrob(), p3, grid::nullGrob(), 
                     layout_matrix = m)

ggsave("prelim_graphics/Drug.Histograms.png", plot)