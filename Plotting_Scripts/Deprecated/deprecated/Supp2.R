library(data.table)
library(ggplot2)
library(ggpubr)
library(operators)
library(gridExtra)
library(RColorBrewer)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive']
C2 = function_df[Assembly =='simple_screening' & as.numeric(Transfer)>0]
C2[,rank :=frank(-CommunityPhenotype),by=list(SpeciesPool,Transfer)]
C2[Transfer ==1]
C2_Plot = C2[Transfer ==1]
C2_Plot$offspring_rank = C2[Transfer ==40]$rank
C2_Plot[,Spearmans:=cor(rank,offspring_rank),by=list(SpeciesPool)]
C2_Plot = C2_Plot[Well == 'W0']
p1 <- ggplot(C2_Plot,aes(x= Spearmans)) + geom_histogram(fill='#E69F00',binwidth=0.02)  +theme_pubr() +
  labs(x = ' Correlation Coefficient between Rank Function at transfer 1 vs Equilibrium',y='Number of Species Pools')
ggsave('../figure/Supp2.png',p1)