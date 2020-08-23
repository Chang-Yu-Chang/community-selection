rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)
library(png)
library(grid)
se <- function(x) sqrt(var(x)/length(x))
Pmax <- function(filename){
  df = fread(filename)
  return(max(df[Transfer==20]$CommunityPhenotype))
}

Omax <- function(filename){
  df = fread(filename)
  return(max(df[Transfer==40]$CommunityPhenotype))
}

img <- readPNG( "../Plots/Cartoons/Fig2A.png", TRUE)
p1 <- rasterGrob(img, interpolate=TRUE)

mapping_file = fread('../Data/Mapping_Files/input_independent.csv')
mapping_file$comp_file = paste('../Data/Raw/',mapping_file$exp_id,'_composition.txt',sep='')
mapping_file$func_file = paste('../Data/Raw/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]
k = 2

A = mapping_file[seed ==k & directed_selection==TRUE & bottleneck == TRUE & bottleneck_size == 1e-5 & protocol == 'directed_selection',]
fdf = fread(A$func_file)
fdf = fdf[Transfer==20]
t = fdf[CommunityPhenotype == max(CommunityPhenotype)]$Well
cdf = fread(A$comp_file)[Type =='consumer']
parent = cdf[Transfer ==20]
parent$CID = paste(parent$Well,'Parent')
offspring = cdf[Transfer ==40]
offspring$CID  = paste(offspring$Well,'Offspring')
all = rbind(parent,offspring)
mat = dcast(all,ID~CID,value.var = 'Abundance')
mat = as.matrix(mat[,2:ncol(mat)])
mat[is.na(mat)] <- 0
pca_mat = prcomp(mat)
pc_df = data.frame('Community' = rownames(pca_mat$rotation),'PC1' = as.numeric(pca_mat$rotation[,1]),'PC2' = as.numeric(pca_mat$rotation[,2]))
vars = pca_mat$sdev^2/sum(pca_mat$sdev^2)
pc_df$Type =sapply(pc_df$Community,function(x) strsplit(as.character(x),split = " ")[[1]][2])
p1 <- ggplot() + 
  geom_point(pc_df[pc_df$Community != 'W5 Parent',],mapping = aes(x=PC1,y=PC2,col=Type),shape=1,size=1,stroke=1) +
  theme_pubr() + labs(x = 'PC1 (32%)' ,y = paste('PC2(4%)'),col='') + 
  scale_colour_manual(values = c('salmon','lightskyblue1')) +
 geom_point(pc_df[pc_df$Community == 'W5 Parent',],mapping = aes(x=PC1,y=PC2),col='blue3',shape=1,size=2,stroke=1) #+
#  geom_point(pc_df[pc_df$Community == 'W41 Offspring',],mapping = aes(x=PC1,y=PC2),col='red',shape=1,size=2,stroke=1)

ggsave('../Plots/FigS8.png',p1,height=4,width=5)

fdf2 = fread(A$func_file)
fdf2 = fdf2[Transfer ==20 | Transfer==40]
fdf2$Community = paste(fdf2$Well, 'Parent')
fdf2[Transfer==20]$Community = paste(fdf2[Transfer==20]$Well, 'Offspring')
fdf2 =fdf2[,c('Community','CommunityPhenotype')]