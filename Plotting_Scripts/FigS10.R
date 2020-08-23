library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
mapping_file = fread('../Data/Mapping_Files/input_independent.csv')
mapping_file$file = paste('../Data/Raw/',mapping_file$exp_id,'_function.txt',sep='')

mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]
A =merge(rbindlist(lapply(mapping_file[protocol == 'select_top25' & monoculture == FALSE & !is.na(bottleneck_size)]$file,fread)),mapping_file[protocol == 'select_top25' & monoculture == FALSE & !is.na(bottleneck_size)])
B =merge(rbindlist(lapply(mapping_file[protocol == 'pool_top25' & monoculture == FALSE & !is.na(bottleneck_size)]$file,fread)),mapping_file[protocol == 'pool_top25' & monoculture == FALSE & !is.na(bottleneck_size)])

A[,Omax :=max(CommunityPhenotype),by=list(seed,bottleneck_size,Transfer)]
B[,Omax :=max(CommunityPhenotype),by=list(seed,bottleneck_size,Transfer)]
A_parent = A[Transfer==20 & Well == 'W1']
B_parent = B[Transfer==20 & Well == 'W1']
A = A[Transfer==40 & Well == 'W1']
B = B[Transfer==40 & Well == 'W1']

# A = A[bottleneck_size %!in% c(1e-4,1e-5)]
A$Popsize_mean = 1000*A$scale*A$bottleneck_size*A$dilution
A$Pmax = A_parent$Omax

# B = B[bottleneck_size %!in% c(1e-4,1e-5)]
B$Popsize_mean = 1000*B$scale*B$bottleneck_size*B$dilution*0.25*96 # (correcting for pooling top25%)
B$Pmax = B_parent$Omax
p1 <- ggplot(A[A$Popsize_mean == 10],aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8)) + ggtitle('Propagule') +  
  theme(plot.title = element_text(size = 10,colour = 'Purple'))



p2 <- ggplot(B[Popsize_mean == 12  ],aes(x= Pmax,y=Omax)) +
  geom_point(col='grey',size=1,shape=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max](parent)), y = expression(F[max](Offspring))) + 
  scale_x_continuous(limits=c(0,2500),breaks=c(0,2500)) +
  scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))  +
  theme(axis.text = element_text(size=8),axis.title = element_text(size=8)) + ggtitle('Migrant Pool') +
  theme(plot.title = element_text(size = 10,colour = 'Orange'))


ggsave('../Plots/FigS10.png',ggarrange(p1,p2,labels=c('A','B')),height=3,width=6)