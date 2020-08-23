library(data.table)
library(ggplot2)
library(ggpubr)
library(operators)
library(gridExtra)
library(RColorBrewer)
library(car)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive' & Assembly %in% c("pool_top25","select_top25",
  "pool_top25_bottleneck_10","select_top25_bottleneck_10",
  "pool_top25_bottleneck_100","select_top25_bottleneck_100",
  "pool_top25_bottleneck_1000","select_top25_bottleneck_1000",
  "pool_top25_bottleneck_10000","select_top25_bottleneck_10000",
  "pool_top25_bottleneck_100000","select_top25_bottleneck_100000",
  "pool_top25_bottleneck_1000000","select_top25_bottleneck_1000000"),]

A = function_df[Assembly %in% c('pool_top25_bottleneck_10',
                                'pool_top25_bottleneck_100',
                                'pool_top25_bottleneck_1000',
                                'pool_top25_bottleneck_10000',
                                'pool_top25_bottleneck_100000',
                                'pool_top25_bottleneck_1000000'),]
A[,maximum :=max(CommunityPhenotype),by=list(SpeciesPool,Transfer,Assembly)]
A[,variance :=var(CommunityPhenotype),by=list(SpeciesPool,Transfer,Assembly)]
A[,Richness := max(Richness),by=list(SpeciesPool,Transfer,Assembly)]

A_Plot = A[Transfer ==20]
A_Plot$offspring_maximum = A[Transfer==40]$maximum
A_Plot$offspring_Variance = A[Transfer==40]$variance
A_Plot$offspring_Richness = A[Transfer==40]$Richness

A_Plot$Bottleneck = 10
A_Plot[Assembly == 'pool_top25_bottleneck_100']$Bottleneck = 100
A_Plot[Assembly == 'pool_top25_bottleneck_1000']$Bottleneck = 1000
A_Plot[Assembly == 'pool_top25_bottleneck_10000']$Bottleneck = 10000
A_Plot[Assembly == 'pool_top25_bottleneck_100000']$Bottleneck = 100000
A_Plot[Assembly == 'pool_top25_bottleneck_1000000']$Bottleneck = 1000000

p1 <- ggplot(A_Plot[Bottleneck!=1000000],aes(x= maximum,y=offspring_maximum)) +
  geom_point(col='grey',size=1) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Pooling', y ='Max Function After Pooling') + 
  scale_x_continuous(limits=c(-350,1600),breaks=c(0,1000)) +
  scale_y_continuous(limits=c(-350,1600),breaks=c(0,1000)) +
  facet_grid(~Bottleneck)


B = function_df[Assembly %in% c('select_top25_bottleneck_10',
                                'select_top25_bottleneck_100',
                                'select_top25_bottleneck_1000',
                                'select_top25_bottleneck_10000',
                                'select_top25_bottleneck_100000',
                                'select_top25_bottleneck_1000000'),]
B[,maximum :=max(CommunityPhenotype),by=list(SpeciesPool,Transfer,Assembly)]
B[,variance :=var(CommunityPhenotype),by=list(SpeciesPool,Transfer,Assembly)]
B[,Richness := max(Richness),by=list(SpeciesPool,Transfer,Assembly)]

B_Plot = B[Transfer ==20]
B_Plot$offspring_maximum = B[Transfer==40]$maximum
B_Plot$offspring_Variance = B[Transfer==40]$variance
B_Plot$offspring_Richness = B[Transfer==40]$Richness

B_Plot$Bottleneck = 10
B_Plot[Assembly == 'select_top25_bottleneck_100']$Bottleneck = 100
B_Plot[Assembly == 'select_top25_bottleneck_1000']$Bottleneck = 1000
B_Plot[Assembly == 'select_top25_bottleneck_10000']$Bottleneck = 10000
B_Plot[Assembly == 'select_top25_bottleneck_100000']$Bottleneck = 100000
B_Plot[Assembly == 'select_top25_bottleneck_1000000']$Bottleneck = 1000000
p2 <- ggplot(B_Plot[Bottleneck!=1000000],aes(x= maximum,y=offspring_maximum)) +
  geom_point(col='grey',size=1) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Selection', y ='Max Function After Selection') + 
  scale_x_continuous(limits=c(-350,1500),breaks=c(0,1000)) +
  scale_y_continuous(limits=c(-350,1500),breaks=c(0,1000)) +
  facet_grid(~Bottleneck)

