rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)

mapping_file = fread('../data/input_independent.csv')
mapping_file$file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')

mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]

A =merge(rbindlist(lapply(mapping_file[protocol == 'simple_screening' & monoculture == FALSE]$file,fread)),mapping_file)
h_df = data.table()
for(k in 1:40){
  for(j in unique(A$seed)){
    parent = A[Transfer ==k-1 & seed == j,]$CommunityPhenotype
    offspring = A[Transfer ==k & seed == j,]$CommunityPhenotype
    h_df = rbind(h_df,data.table(Transfer = k, seed = j, Heritability = cov(parent,offspring)/var(offspring)))
  }
}

p1 <- ggplot(h_df[Transfer>1],aes(x=Transfer,y=Heritability)) + geom_jitter(height=0,width=0.2,col='grey',shape=1) + theme_pubr() + labs(x ='Generation',y=expression(h^2))
ggsave('../Plots/FigS7.png',p1,width=6,height=5)


