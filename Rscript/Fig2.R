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


mapping_file = fread('../data/input_independent.csv')
mapping_file$file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]
k = 2

A = mapping_file[directed_selection==TRUE & bottleneck == TRUE & bottleneck_size == 1e-5 & protocol == 'directed_selection',]
A =merge(rbindlist(lapply(A$file,fread)),A)
A[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
top = A[rank==1 & seed==k & Transfer ==20]
A[Transfer>20]$Well = paste(A[Transfer>20]$Well,'_2',sep='')
fake_A = A[Transfer ==21]
fake_A$Transfer =fake_A$Transfer-1
fake_A$CommunityPhenotype = top$CommunityPhenotype
A = rbind(A,fake_A)
top2 = A[rank==1 & seed==k & Transfer ==40]

A$Highlight = A$Well == top$Well | A$Well == top2$Well
ctrl_max = max(fread(mapping_file[protocol =='simple_screening' & seed==k & monoculture == FALSE]$file)[Transfer==40]$CommunityPhenotype)
t_max = max(A[seed==k & Transfer == 40]$CommunityPhenotype)
img <- readPNG( "../Plots/Cartoons/Fig2B.png", TRUE)
p2_inset <- rasterGrob(img, interpolate=TRUE)
p2 <- ggplot() + 
  geom_line(A[Transfer>0 & seed==k & Highlight == FALSE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray85') + 
  geom_line(A[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20') + 
  geom_point(A[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20',size=0.5) + 
  theme_pubr() + labs(y='F',x='Generation')+ 
  geom_hline(yintercept= ctrl_max,linetype =2,col='Red')  +
  scale_y_continuous(breaks=c(-1000,1000),limits=c(-1000,1300))+
  scale_x_continuous(breaks=c(0,20,40)) + theme(axis.text = element_text(size=8),axis.title = element_text(size=10)) +
  annotation_custom(p2_inset,xmin=19,xmax=21,ymin=750,ymax=1100) +
  annotate('text',x=13,y=1300,label=expression(F[max](Parent)),size=3)  +
  annotate('segment',x=20,xend=15,y=ctrl_max,yend=1200,linetype = 2,col = '#7570B3') +
  annotate('text',x=33,y=1300,label=expression(F[max](Offspring)),size=3)   +
  annotate('segment',x=40,xend=35,y=t_max,yend=1200,linetype = 2,col = '#7570B3')
  
  



B = mapping_file[directed_selection==TRUE & bottleneck == TRUE  & protocol == 'directed_selection']
B =merge(rbindlist(lapply(B$file,fread)),B)
B[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer,bottleneck_size)]
B = B[bottleneck_size %!in% c(1e-4,1e-5)]

top = B[Transfer==20]
top[, max := max(CommunityPhenotype),by = list(seed,bottleneck_size)] 
top = top[max == CommunityPhenotype]
B = B[Transfer==40]
B$Popsize_mean = 1000*B$scale*B$bottleneck_size*B$dilution

B = B[paste(Well,exp_id) %!in% paste(top$Well,top$exp_id)]
B[,F_Mean := mean(CommunityPhenotype),by = list(seed,Transfer,bottleneck_size)]
B[,F_Max := max(CommunityPhenotype),by = list(seed,Transfer,bottleneck_size)]

p3 <- ggplot(B[seed==k],aes(x=Popsize_mean,y=CommunityPhenotype)) + geom_jitter(height=0,width=0.1,shape=1,col='grey70') + 
  geom_line(aes(y=F_Mean),linetype=2,size=1,col='Purple') +
  geom_line(aes(y=F_Max),linetype=2,size=1,col='Orange') +
  xscale("log2",.format=FALSE) +
  theme_pubr() +
  labs(x = '<Population  Size> after bottleneck',
       y = 'F') + theme(axis.title = element_text(size=10),axis.text = element_text(size=8))


B_Summary = B[Well == 'W0']
B_Summary[,Mean_F_Mean := mean(F_Mean),by = list(Popsize_mean)]
B_Summary[,SD_F_Mean := sd(F_Mean),by = list(Popsize_mean)]

B_Summary[,Mean_F_Max := mean(F_Max),by = list(Popsize_mean)]
B_Summary[,SD_F_Max := sd(F_Max),by = list(Popsize_mean)]

p3_inset <- ggplot() + 
  geom_line(B_Summary,mapping = aes(x=Popsize_mean,y=Mean_F_Mean),col='Purple') + 
  geom_ribbon(B_Summary,mapping = aes(x=Popsize_mean,ymax=Mean_F_Mean +SD_F_Mean,ymin = Mean_F_Mean -SD_F_Mean),fill='Purple',alpha=0.2) + 
  geom_line(B_Summary,mapping = aes(x=Popsize_mean,y=Mean_F_Max),col='Orange') + 
  geom_ribbon(B_Summary,mapping = aes(x=Popsize_mean,ymax=Mean_F_Max + SD_F_Max,ymin = Mean_F_Max - SD_F_Max),fill='Orange',alpha=0.2) + 
  xscale("log2",.format=FALSE) +
  theme_pubr()  + labs(x='',y='') +
  theme(axis.text = element_blank())  

p3 = p3 + annotation_custom(arrangeGrob(p3_inset),xmin=5,xmax=17,ymin=-450,ymax=550)



C = mapping_file[directed_selection==TRUE & bottleneck == TRUE & bottleneck_size == 1e-5 & protocol == 'directed_selection']
D = mapping_file[directed_selection==TRUE & knock_in == TRUE & knock_in_threshold == 0.95]
E = mapping_file[directed_selection==TRUE & knock_out == TRUE]
F = mapping_file[directed_selection==TRUE & migration ==TRUE & is.na(s_migration)]
G = mapping_file[directed_selection==TRUE & coalescence == TRUE]
H = mapping_file[directed_selection==TRUE & resource_shift == TRUE & r_percent == 0.5]

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

p4b <- rasterGrob(readPNG( "../Plots/Cartoons/Bottleneck.png", TRUE), interpolate=TRUE)
p5b <- rasterGrob(readPNG( "../Plots/Cartoons/Knockin.png", TRUE), interpolate=TRUE)
p6b <- rasterGrob(readPNG( "../Plots/Cartoons/Knockout.png", TRUE), interpolate=TRUE)
p7b <- rasterGrob(readPNG( "../Plots/Cartoons/Migration.png", TRUE), interpolate=TRUE)
p8b <- rasterGrob(readPNG( "../Plots/Cartoons/Coalescence.png", TRUE), interpolate=TRUE)
p9b <- rasterGrob(readPNG( "../Plots/Cartoons/Resource.png", TRUE), interpolate=TRUE)

p4 <- ggplot() +
  geom_point(C[seed!=k],mapping = aes(x= Pmax,y=Omax),col='grey',size=1,shape=1) +
  geom_point(C[seed==k],mapping = aes(x= Pmax,y=Omax),col='Black',size=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  annotation_custom(p4b,xmin=1000,xmax=2600,ymin=0,ymax=1000)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))


p5 <- ggplot(D,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  annotation_custom(p5b,xmin=1000,xmax=2600,ymin=0,ymax=1000)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))


p6 <- ggplot(E,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  annotation_custom(p6b,xmin=1000,xmax=2600,ymin=0,ymax=1000)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))


p7 <- ggplot(F,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  annotation_custom(p7b,xmin=1000,xmax=2600,ymin=0,ymax=1000)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))

p8 <- ggplot(G,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  annotation_custom(p8b,xmin=1000,xmax=2600,ymin=0,ymax=1000)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))

p9 <- ggplot(H,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  annotation_custom(p9b,xmin=1000,xmax=2600,ymin=0,ymax=1000)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))
bottom = ggarrange(p4,p5,p6,p7,p8,p9,nrow=2,ncol=3,labels=c('D','E','F','G','H','I'))
right = ggarrange(p2,p3,labels=c('B','C'),nrow=2,ncol=1)
ggsave('../Plots/Fig2.png',ggarrange(ggarrange(p1,right,labels=c('A'),widths=c(2,1)),bottom,ncol=1,nrow=2,heights=c(6,6)),width=8,height=8)