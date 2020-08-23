rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)

mapping_file = fread('../data/input_independent.csv')
mapping_file$comp_file = paste('../data/raw/',mapping_file$exp_id,'_composition.txt',sep='')
mapping_file$func_file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')

mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]
k = 2 #Seed for plotting
mono = mapping_file[seed == k & monoculture == TRUE]
pooled = mapping_file[seed == k & protocol == 'pool_top25' & monoculture == FALSE & is.na(bottleneck_size)]
compositiondf =fread(pooled$comp_file)[Type == 'consumer']
functiondf = fread(pooled$func_file)

top_well = functiondf[Transfer ==20]
top_well_before = top_well[CommunityPhenotype==max(CommunityPhenotype)]$Well
top_well = functiondf[Transfer ==40]
top_well_after = top_well[CommunityPhenotype==max(CommunityPhenotype)]$Well

before_pooling = compositiondf[Transfer == 20 & Well == top_well_before]
after_pooling = compositiondf[Transfer == 40 & Well == top_well_after]

monoculture = fread(mono$func_file)[Transfer==0]
monoculture$ID = as.numeric(sub('.','',monoculture$Well))
monoculture$Phi = monoculture$CommunityPhenotype/monoculture$Biomass
before_pooling$Phi = monoculture[match(before_pooling$ID,monoculture$ID)]$Phi
after_pooling$Phi = monoculture[match(after_pooling$ID,monoculture$ID)]$Phi
before_pooling$Time = 'Before Pooling'
after_pooling$Time = 'After Pooling'
all = rbind(before_pooling,after_pooling)
all$PhiN = all$Phi*all$Abundance

p1 <- ggboxplot(all,x='Time',y='Phi',col='Time',palette = "dark2",
                add = "jitter",legend='right',shape=1,outlier.size=1,outlier.colour='white')+ 
  scale_colour_manual(values =c('#D95F02','#7570B3')) +
  labs(x='',y=expression(Phi[i]),col='') +

  theme(axis.text.x=element_text(angle=-45,hjust=0.3,colour=c('#D95F02','#7570B3')),
        axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0)))   +
  theme(legend.position = "none") 
p2 <- ggboxplot(all,x='Time',y='PhiN',col='Time',palette = "dark2",
                add = "jitter",legend='right',shape=1,outlier.size=1,outlier.colour='white') +  
  scale_colour_manual(values =c('#D95F02','#7570B3')) + 
  labs(x='',y=expression(Phi[i]*N[i])) +
  theme(axis.text.x=element_text(angle=-45,hjust=0.3,colour=c('#D95F02','#7570B3')),
        axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0)))  +
  theme(legend.position = "none")  +
  annotate('segment',x=1.165,xend=2,y=332.98741,yend=80.63878,linetype = 2,col ='Black') +
  geom_point(aes(x=1.8, y=-127.4592), colour='#7570B3')
ggsave('../Plots/FigS6.png',ggarrange(p1,p2,labels=c('A','B')),width=6,height=4)
