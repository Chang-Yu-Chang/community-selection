library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)
se <- function(x) sqrt(var(x)/length(x))
Pmax <- function(filename){
  df = fread(filename)
  return(max(df[Transfer==20]$CommunityPhenotype))
}

Omax <- function(filename){
  df = fread(filename)
  return(max(df[Transfer==40]$CommunityPhenotype))
}
mapping_file = fread('../data/input_independent.csv')
mapping_file$file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[cost_mean==0 & selected_function == 'f2_interaction',]
df_b = mapping_file[protocol %in% c('Blouin2015','Jochum2019',
                                    'Mueller2019','Panke_Buisse2015',
                                    'Swenson2000a','Swenson2000b',
                                    'Chang2020a',
                                    'Chang2020b','Arora2019',
                                    'Raynaud2019a','Raynaud2019b','Wright2019')]
df_b$protocol = factor(df_b$protocol,levels=c('Swenson2000a','Blouin2015',
                                              'Panke_Buisse2015','Jochum2019',
                                              'Raynaud2019b','Mueller2019',
                                              'Wright2019','Swenson2000b','Arora2019','Raynaud2019a','Chang2020a','Chang2020b'))
df_b$Performance_Max = NA
df_b$Performance_Mean = NA

for(j in 1:nrow(df_b)){
  k = df_b$seed[j]
  screen = fread(mapping_file[seed==k & protocol=='simple_screening' & monoculture==FALSE]$file)
  ref_max = max(screen[Transfer==40]$CommunityPhenotype)
  ref_mean = mean(screen[Transfer==40]$CommunityPhenotype)
  temp_df = fread(df_b$file[j])[Transfer==40]
  df_b$Performance_Max[j] = max(temp_df$CommunityPhenotype) -ref_max
  df_b$Performance_Mean[j] = mean(temp_df$CommunityPhenotype) - ref_mean
  
}
df_b$Method = 'Migrant Pool'
df_b[protocol%in% c('Chang2020a','Chang2020b','Swenson2000b','Arora2019','Raynaud2019a')]$Method = 'Propagule'
p1 <- ggplot(df_b,
             aes(x=protocol,y=Performance_Mean,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text =element_text(size=8,angle=-90),axis.title=element_text(size=10)) + 
  scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression( Mean(AS) - Mean(NS))) #+ scale_y_continuous(breaks=c(2000,0,-2000),limits=c(-2000,2000))


p2 <- ggplot(df_b,
             aes(x=protocol,y=Performance_Max,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=8,angle=-90),axis.title=element_text(size=10)) + 
  scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression(Q==F[max](AS) - F[max](NS) ))# + scale_y_continuous(breaks=c(0,-2000,2000),limits=c(-2000,2000))



C = mapping_file[directed_selection==TRUE & bottleneck == TRUE & bottleneck_size == 1e-5 & protocol == 'directed_selection']
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

p4b <- rasterGrob(readPNG( "../Plots/Cartoons/Bottleneck.png", TRUE), interpolate=TRUE)
p5b <- rasterGrob(readPNG( "../Plots/Cartoons/Knockin.png", TRUE), interpolate=TRUE)
p6b <- rasterGrob(readPNG( "../Plots/Cartoons/Knockout.png", TRUE), interpolate=TRUE)
p7b <- rasterGrob(readPNG( "../Plots/Cartoons/Migration.png", TRUE), interpolate=TRUE)
p8b <- rasterGrob(readPNG( "../Plots/Cartoons/Coalescence.png", TRUE), interpolate=TRUE)
p9b <- rasterGrob(readPNG( "../Plots/Cartoons/Resource.png", TRUE), interpolate=TRUE)

p4 <- ggplot(C,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,5e+5),breaks=c(0,5e+5)) +
  scale_y_continuous(limits=c(0,5e+5),breaks=c(0,5e+5))  +
  annotation_custom(p4b,xmin=2e+5,xmax=5e+5,ymin=0,ymax=2e+5)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))


p5 <- ggplot(D,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,5e+5),breaks=c(0,5e+5)) +
  scale_y_continuous(limits=c(0,5e+5),breaks=c(0,5e+5))  +
  annotation_custom(p5b,xmin=2e+5,xmax=5e+5,ymin=0,ymax=2e+5)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))


p6 <- ggplot(E,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,5e+5),breaks=c(0,5e+5)) +
  scale_y_continuous(limits=c(0,5e+5),breaks=c(0,5e+5))  +
  annotation_custom(p6b,xmin=2e+5,xmax=5e+5,ymin=0,ymax=2e+5)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))


p7 <- ggplot(F,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,5e+5),breaks=c(0,5e+5)) +
  scale_y_continuous(limits=c(0,5e+5),breaks=c(0,5e+5))  +
  annotation_custom(p7b,xmin=2e+5,xmax=5e+5,ymin=0,ymax=2e+5)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))

p8 <- ggplot(F,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,5e+5),breaks=c(0,5e+5)) +
  scale_y_continuous(limits=c(0,5e+5),breaks=c(0,5e+5))  +
  annotation_custom(p8b,xmin=2e+5,xmax=5e+5,ymin=0,ymax=2e+5)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))

p9 <- ggplot(H,aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  scale_x_continuous(limits=c(0,5e+5),breaks=c(0,5e+5)) +
  scale_y_continuous(limits=c(0,5e+5),breaks=c(0,5e+5))  +
  annotation_custom(p9b,xmin=2e+5,xmax=5e+5,ymin=0,ymax=2e+5)+ 
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8))

ggsave('../Plots/FigS2.png',grid.arrange(ggarrange(p1,p2,labels=c('A','B'),common.legend=TRUE), ggarrange(p4,p5,p6,p7,p8,p9,labels=c('C','D','E','F','G','H')),nrow=2,ncol=1,heights=c(3,3)),height=8,width=8)