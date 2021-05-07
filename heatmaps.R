library(tidyverse)
library(reshape2)
library(gridExtra)
FC <- read.csv("derived_data/FC.clean.csv")

alc.user <- 131722
drug.user <- 358144
control <- 185947

#this will give us the upper triangles for the FC matrices
alc.upp <- FC %>% 
  filter(Subject == alc.user) %>% 
  select(-Subject)

drug.upp <- FC %>% 
  filter(Subject == drug.user) %>% 
  select(-Subject)

control.upp <- FC %>% 
  filter(Subject == control) %>% 
  select(-Subject)

#now we can rebuild the matrices
alc.mat.temp <- matrix(0, 68, 68)
alc.mat.temp[upper.tri(alc.mat.temp, diag = F)] <- as.numeric(na.omit(alc.upp))
alc.mat <- alc.mat.temp + t(alc.mat.temp)

drug.mat.temp <- matrix(0, 68, 68)
drug.mat.temp[upper.tri(drug.mat.temp, diag = F)] <- as.numeric(na.omit(drug.upp))
drug.mat <- drug.mat.temp + t(drug.mat.temp)

ctrl.mat.temp <- matrix(0, 68, 68)
ctrl.mat.temp[upper.tri(ctrl.mat.temp, diag = F)] <- as.numeric(na.omit(control.upp))
ctrl.mat <- ctrl.mat.temp + t(ctrl.mat.temp)

image(drug.mat)
image(ctrl.mat)
image(alc.mat)

#so to do heatmaps what we need to do is melt these matrices
drug.melt <- melt(drug.mat)
alc.melt <- melt(alc.mat)
ctrl.melt <- melt(ctrl.mat)

p1 <- drug.melt %>% 
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_distiller(palette = "Spectral", name = "Value", breaks = c(0, .25, .5, .75)) +
  theme_void() +
  labs(title = "Hard Drug User FC Matrix") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- alc.melt %>% 
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_distiller(palette = "Spectral", name = "Value", breaks = c(0, .25, .5, .75)) +
  theme_void() +
  labs(title = "Alcohol User FC Matrix") +
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ctrl.melt %>% 
  ggplot(aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_distiller(palette = "Spectral", name = "Value", breaks = c(0, .25, .5, .75)) +
  theme_void() +
  labs(title = "Non Substance User FC Matrix") +
  theme(plot.title = element_text(hjust = 0.5))

m <- matrix(c(1, 1, 2, 2, 3, 4, 4, 5), ncol = 4, byrow = T)
plot <- grid.arrange(p1, p2, grid::nullGrob(), p3, grid::nullGrob(), 
             layout_matrix = m)

ggsave("derived_graphics/FC_Heatmaps.png", plot = plot)