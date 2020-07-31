rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
mapping_file = fread('../data/input_independent.csv')
mapping_file$file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]

mapping_file$Max = NA
mapping_file$Mean = NA
for(i in 1:nrow(mapping_file)){
  t = fread(mapping_file$file[i])
  mapping_file$Max[i] = max(t[Transfer == mapping_file$n_transfer[i]]$CommunityPhenotype)
  mapping_file$Mean[i] = mean(t[Transfer == mapping_file$n_transfer[i]]$CommunityPhenotype)
}

experiment = mapping_file[protocol %in% c('Swenson2000a','Swenson2000b',
                                   'Chang2020a','Chang2020b',
                                   'Arora2019','Raynaud2019a',
                                   'Raynaud2019b',
                                   'Blouin2015','Wright2019')]
control = mapping_file[protocol %in% c('Swenson2000a_control','Swenson2000b_control',
                                   'Chang2020a_control','Chang2020b_control',
                                   'Arora2019_control','Raynaud2019a_control',
                                   'Raynaud2019b_control',
                                   'Blouin2015_control','Wright2019_control')]
experiment$Performance_Max = experiment$Max - control$Max
experiment$Performance_Mean = experiment$Mean - control$Mean

p1 <- ggplot(experiment,
             aes(x=protocol,y=Performance_Mean)) + 
  geom_boxplot(outlier.shape = NA,col='grey') +  
  geom_jitter(height=0,col='grey',shape=4)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90)) + 
  labs(x = '',y=expression( Mean(AS) - Mean(RS))) + scale_y_continuous(breaks=c(1000,0,-1000),limits=c(-1000,1500))

p2 <- ggplot(experiment,
             aes(x=protocol,y=Performance_Max)) + 
  geom_boxplot(outlier.shape = NA,col='grey') +  
  geom_jitter(height=0,col='grey',shape=4)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90)) + 

  labs(x = '',y=expression(F[max](AS) - F[max](RS))) + scale_y_continuous(breaks=c(1000,0,-1000),limits=c(-1000,1500))

ggsave('../Plots/FigS2.png',ggarrange(p1,p2,common.legend = TRUE,labels='AUTO'),height=5,width=10)

for(k in unique(experiment$protocol)){
  print(t.test(experiment$Performance_Max))
  print(t.test(experiment$Performance_Mean))
  
}