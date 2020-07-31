rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)
mapping_file = fread('../data/raw/input_additive.csv')
mapping_file$file = paste('../data/raw/additive/',mapping_file$exp_id,'_function.txt',sep='')


Pmax <- function(filename){
  df = fread(filename)
  return(max(df[Transfer==20]$CommunityPhenotype))
}

Omax <- function(filename){
  df = fread(filename)
  return(max(df[Transfer==40]$CommunityPhenotype))
}
C = mapping_file[directed_selection==TRUE & bottleneck == TRUE & bottleneck_size == 1e-5]
D = mapping_file[directed_selection==TRUE & knock_in == TRUE & knock_in_threshold == 0.95]
E = mapping_file[directed_selection==TRUE & knock_out == TRUE]
F = mapping_file[directed_selection==TRUE & migration ==TRUE & is.na(s_migration)]
G = mapping_file[directed_selection==TRUE & coalescence == TRUE]
H = mapping_file[directed_selection==TRUE & resource_shift == TRUE & r_percent == 1]

C$Pmax = sapply(C$file,Pmax)
C$Omax = sapply(C$file,Omax)
D$Pmax = sapply(D$file,Pmax)
D$Omax = sapply(D$file,Omax)
E$Pmax = sapply(E$file,Pmax)
E$Omax = sapply(E$file,Omax)
F$Pmax = sapply(F$file,Pmax)
F$Omax = sapply(F$file,Omax)
G$Pmax = sapply(G$file,Pmax)
G$Omax = sapply(G$file,Omax)
H$Pmax = sapply(H$file,Pmax)
H$Omax = sapply(H$file,Omax)

p1b <- rasterGrob(readPNG( "../Plots/Cartoons/Bottleneck.png", TRUE), interpolate=TRUE)
p2b <- rasterGrob(readPNG( "../Plots/Cartoons/Knockin.png", TRUE), interpolate=TRUE)
p3b <- rasterGrob(readPNG( "../Plots/Cartoons/Knockout.png", TRUE), interpolate=TRUE)
p4b <- rasterGrob(readPNG( "../Plots/Cartoons/Migration.png", TRUE), interpolate=TRUE)
p5b <- rasterGrob(readPNG( "../Plots/Cartoons/Coalescence.png", TRUE), interpolate=TRUE)
p6b <- rasterGrob(readPNG( "../Plots/Cartoons/Resource.png", TRUE), interpolate=TRUE)

p1 <- ggplot(C,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=2) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  scale_y_continuous(limits=c(500,2500),breaks=c(1000,2000))  +
  annotation_custom(p1b,xmin=1300,xmax=2500,ymin=300,ymax=1500)


p2 <- ggplot(D,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=2) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  scale_y_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  annotation_custom(p2b,xmin=1300,xmax=2500,ymin=300,ymax=1500)


p3 <- ggplot(E,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=2) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  scale_y_continuous(limits=c(500,2500),breaks=c(1000,2000))  +
  annotation_custom(p3b,xmin=1300,xmax=2500,ymin=300,ymax=1500)


p4 <- ggplot(F,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=2) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  scale_y_continuous(limits=c(500,2500),breaks=c(1000,2000))  +
  annotation_custom(p4b,xmin=1300,xmax=2500,ymin=300,ymax=1500)


p5 <- ggplot(F,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=2) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  scale_y_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  annotation_custom(p5b,xmin=1300,xmax=2500,ymin=300,ymax=1500)

p6 <- ggplot(H,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=2) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Directed Selection', y ='Max Function Directed Selection') + 
  scale_x_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  scale_y_continuous(limits=c(500,2500),breaks=c(1000,2000)) +
  annotation_custom(p6b,xmin=1300,xmax=2500,ymin=300,ymax=1500)

ggsave('../Plots/Fig4.png',ggarrange(p1,p2,p3,p4,p5,p6,labels='AUTO'),height=8,width=12)