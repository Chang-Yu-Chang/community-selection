library(data.table)
library(ggplot2)
library(ggpubr)
library(gridExtra)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive' & Assembly %in% c("coalescence",
                                                                                    "bottleneck",
                                                                                    "knock_in",
                                                                                    "knock_out",
                                                                                    "migration",
                                                                                    "resource_add")]
Parent_Max = c()
Offspring_Max = c()
SpeciesPool =c()
Pertubation =c()
for(k in unique(function_df$SpeciesPool)){
  for(j in unique(function_df$Assembly)){
  Parent_Max = c(Parent_Max,max(function_df[SpeciesPool ==k  & Transfer == 20 & Assembly ==j]$CommunityPhenotype))
  Offspring_Max = c(Offspring_Max,max(function_df[SpeciesPool ==k  & Transfer == 40 & Assembly ==j]$CommunityPhenotype))
  SpeciesPool = c(SpeciesPool,k)
  Pertubation = c(Pertubation,j)
  }
}
Fig2df = data.table(Parent_Max,Offspring_Max,SpeciesPool,Pertubation)
p1 <- ggplot(Fig2df[Pertubation == 'bottleneck'],aes(x= Parent_Max,y=Offspring_Max)) +
  geom_point(col='grey',size=2) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(350,1505),breaks=c(500,1500)) +
  scale_y_continuous(limits=c(350,1505),breaks=c(500,1500))

p2 <- ggplot(Fig2df[Pertubation == 'knock_in'],aes(x= Parent_Max,y=Offspring_Max)) +
  geom_point(col='grey',size=2) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(350,1505),breaks=c(500,1500)) +
  scale_y_continuous(limits=c(350,1505),breaks=c(500,1500))


p3 <- ggplot(Fig2df[Pertubation == 'knock_out'],aes(x= Parent_Max,y=Offspring_Max)) +
  geom_point(col='grey',size=2) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(350,1505),breaks=c(500,1500)) +
  scale_y_continuous(limits=c(350,1505),breaks=c(500,1500))


p4 <- ggplot(Fig2df[Pertubation == 'coalescence'],aes(x= Parent_Max,y=Offspring_Max)) +
  geom_point(col='grey',size=2) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(350,1505),breaks=c(500,1500)) +
  scale_y_continuous(limits=c(350,1505),breaks=c(500,1500))


p5 <- ggplot(Fig2df[Pertubation == 'migration'],aes(x= Parent_Max,y=Offspring_Max)) +
  geom_point(col='grey',size=2) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(350,1505),breaks=c(500,1500)) +
  scale_y_continuous(limits=c(350,1505),breaks=c(500,1500))


p6 <- ggplot(Fig2df[Pertubation == 'resource_add'],aes(x= Parent_Max,y=Offspring_Max)) +
  geom_point(col='grey',size=2) +
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(350,1505),breaks=c(500,1500)) +
  scale_y_continuous(limits=c(350,1505),breaks=c(500,1500))

a1 = fread('../data/raw/botttleneck/bottleneck-1-f1_additive-TPostBottleneck-function.txt')
a2 = fread('../data/raw/botttleneck/bottleneck-2-f1_additive-TPostBottleneck-function.txt')
a3 = fread('../data/raw/botttleneck/bottleneck-4-f1_additive-TPostBottleneck-function.txt')
a4 = fread('../data/raw/botttleneck/bottleneck-8-f1_additive-TPostBottleneck-function.txt')
a5 = fread('../data/raw/botttleneck/bottleneck-16-f1_additive-TPostBottleneck-function.txt')
a6 = fread('../data/raw/botttleneck/bottleneck-32-f1_additive-TPostBottleneck-function.txt')
a7 = fread('../data/raw/botttleneck/bottleneck-64-f1_additive-TPostBottleneck-function.txt')
a8 = fread('../data/raw/botttleneck/bottleneck-128-f1_additive-TPostBottleneck-function.txt')
a9 = fread('../data/raw/botttleneck/bottleneck-256-f1_additive-TPostBottleneck-function.txt')
a10 = fread('../data/raw/botttleneck/bottleneck-512-f1_additive-TPostBottleneck-function.txt')
a11 = fread('../data/raw/botttleneck/bottleneck-1024-f1_additive-TPostBottleneck-function.txt')
a12 = fread('../data/raw/botttleneck/bottleneck-2048-f1_additive-TPostBottleneck-function.txt')


b1 <- fread('../data/raw/botttleneck/bottleneck-1-f1_additive-T40-function.txt')
b1$Pop = a1$Biomass*1e+6
b1$Pop2 = mean(a1$Biomass*1e+6)

b2 <- fread('../data/raw/botttleneck/bottleneck-2-f1_additive-T40-function.txt')
b2$Pop = a2$Biomass*1e+6
b2$Pop2 = mean(a2$Biomass*1e+6)

b3 <- fread('../data/raw/botttleneck/bottleneck-4-f1_additive-T40-function.txt')
b3$Pop = a3$Biomass*1e+6
b3$Pop2 = mean(a3$Biomass*1e+6)

b4 <- fread('../data/raw/botttleneck/bottleneck-8-f1_additive-T40-function.txt')
b4$Pop = a4$Biomass*1e+6
b4$Pop2 = mean(a4$Biomass*1e+6)

b5 <- fread('../data/raw/botttleneck/bottleneck-16-f1_additive-T40-function.txt')
b5$Pop =a5$Biomass*1e+6
b5$Pop2 =mean(a5$Biomass*1e+6)

b6 <- fread('../data/raw/botttleneck/bottleneck-32-f1_additive-T40-function.txt')
b6$Pop= a6$Biomass*1e+6
b6$Pop2= mean(a6$Biomass*1e+6)

b7 <- fread('../data/raw/botttleneck/bottleneck-64-f1_additive-T40-function.txt')
b7$Pop= a7$Biomass*1e+6
b7$Pop2= mean(a7$Biomass*1e+6)

b8 <- fread('../data/raw/botttleneck/bottleneck-128-f1_additive-T40-function.txt')
b8$Pop= a8$Biomass*1e+6
b8$Pop2= mean(a8$Biomass*1e+6)

b9 <- fread('../data/raw/botttleneck/bottleneck-256-f1_additive-T40-function.txt')
b9$Pop= a9$Biomass*1e+6
b9$Pop2= mean(a9$Biomass*1e+6)

b10 <- fread('../data/raw/botttleneck/bottleneck-512-f1_additive-T40-function.txt')
b10$Pop= a10$Biomass*1e+6
b10$Pop2= mean(a10$Biomass*1e+6)

b11 <- fread('../data/raw/botttleneck/bottleneck-1024-f1_additive-T40-function.txt')
b11$Pop= a11$Biomass*1e+6
b11$Pop2= mean(a11$Biomass*1e+6)

b12 <- fread('../data/raw/botttleneck/bottleneck-2048-f1_additive-T40-function.txt')
b12$Pop= a12$Biomass*1e+6
b12$Pop2= mean(a12$Biomass*1e+6)


t = rbind(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12)

c = max(fread('../data/raw/botttleneck/bottleneck-1-f1_additive-T20-function.txt')$CommunityPhenotype)
p1 <- ggplot(t[Well != 'W55'],aes(x=Pop,y=CommunityPhenotype)) + geom_point() + 
  xscale("log2",.format=FALSE) +
  theme_pubr() +
  labs(x = 'Population  Size after bottleneck',
       y = 'Community Function')  + geom_hline(yintercept = c,linetype=2,col='red')


p2 <- ggplot(t[Well != 'W55'],aes(x=Pop2,y=CommunityPhenotype)) + geom_point() + 
  xscale("log2",.format=FALSE) +
  theme_pubr() +
  labs(x = '<Population  Size> after bottleneck',
       y = 'Community Function')  + geom_hline(yintercept = c,linetype=2,col='red')

