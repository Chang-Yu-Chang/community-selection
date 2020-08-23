
rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)
library(grid)
library(cowplot)
mapping_file = fread('../data/input_additive_robustness_screen.csv')
mapping_file$file = paste('../data/raw/robustness/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[!(migration == TRUE & is.na(n_migration_ds))] 
mapping_file$Community ='NA'
mapping_file$Pertubation ='NA'

mapping_file[grep('iteration_5.*synthetic_community',mapping_file$exp_id),]$Community = 'Synthetic'
mapping_file[grep('iteration_5.*selected_community',mapping_file$exp_id),]$Community = 'DE'
mapping_file[grep('simple_screening.*selected_community',mapping_file$exp_id),]$Community = 'NS'
mapping_file[grep('migration',mapping_file$exp_id),]$Pertubation = 'Migration'
mapping_file[grep('resource_shift',mapping_file$exp_id),]$Pertubation = 'Resource Shift'
mapping_file[grep('bottleneck',mapping_file$exp_id),]$Pertubation = 'Bottleneck'
mapping_file[grep('knock_out',mapping_file$exp_id),]$Pertubation = 'Species Knock-Out'
mapping_file = mapping_file[Community != '']
mapping_file = mapping_file[Community != 'NA',]
mapping_file = mapping_file[Pertubation != 'NA',]
mapping_file = merge(rbindlist(lapply(mapping_file$file,fread)),mapping_file)

t_final = mapping_file[Transfer==40]
t20 = mapping_file[Transfer==20]
t_final$RS = 1- (2*abs(t20$CommunityPhenotype-t_final$CommunityPhenotype)/(t20$CommunityPhenotype+abs(t20$CommunityPhenotype-t_final$CommunityPhenotype)))
t_final[,Fmax :=max(CommunityPhenotype),by=list(exp_id,Transfer)]
t_final[,R :=mean(RS),by=list(exp_id,Transfer)]
t_final[,Fmean :=mean(CommunityPhenotype),by=list(exp_id,Transfer)]


p6 <- ggplot() + 
  geom_point(t_final[seed==k],mapping = aes(x=R,y=Fmean,col=Community),size= 1,shape=1) +
  geom_point(t_final[seed!=k],mapping = aes(x=R,y=Fmean,col=Community),size= 1,shape=1) + theme_pubr() +
  labs(x= expression(mean(R)),y = expression(mean(F^'*')) ,col='') + #guides(col=FALSE) +
  scale_colour_manual(values = c('#D95F02','#1B9E77','#7570B3'))    + facet_wrap(~Pertubation,scales = "free")

ggsave('../Plots/FigS9.png',p6,width=6,height=6)