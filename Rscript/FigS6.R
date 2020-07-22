rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)

mapping_file = fread('../data/input_independent.csv')
mapping_file$file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]
k = 2 #Seed for plotting
A =merge(rbindlist(lapply(mapping_file[protocol == 'simple_screening' & monoculture == FALSE]$file,fread)),mapping_file)
A[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
A[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
A[,variance :=var(CommunityPhenotype),by=list(seed,Transfer)]
A[,mean :=mean(CommunityPhenotype),by=list(seed,Transfer)]

ctrl_max = max(A[Transfer==40 & seed==k]$CommunityPhenotype)
top = A[Transfer==1 & seed ==k & rank ==1]$Well
A$Highlight = A$Well == top

p1 <- ggplot() + 
  geom_line(A[Transfer>0 & seed==k & Highlight == FALSE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray90') + 
  geom_line(A[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20') + 
  geom_point(A[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20',size=1,shape=5) + 
  theme_pubr() + labs(y='F',x='Generation')+ 
  # geom_hline(yintercept= ctrl_max,linetype =2,col='Red')  +
  scale_y_continuous(breaks=c(-1000,0,1000),limits=c(-1000,1000))+
  scale_x_continuous(breaks=c(0,20,40))  +  
  annotate('text',x=20,y=1000,label='No Selection') 

A$parent_rank = 0
A$parent_F = 0

A[Transfer==40]$parent_rank = A[Transfer==1]$rank
A[Transfer==40]$parent_F = A[Transfer==1]$CommunityPhenotype

p2 <- ggplot() + 
  geom_jitter(A[seed!=k & Transfer==40],mapping = aes(x=parent_rank,y=rank),col='lightskyblue1',size= 0.5,shape=1,height=0.2,width=0.2) +
  geom_jitter(A[seed==k & Transfer==40],mapping = aes(x=parent_rank,y=rank),col='blue3',size= 1,shape=1,stroke=1,height=0.2,width=0.2) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x='Rank F at Start',y ='Rank F at Equilibrium')  + 
  theme_pubr() +  
  labs(shape = 'Species Pool',col = 'Species Pool') +
  scale_x_continuous(breaks= c(1,96)) + scale_y_continuous(breaks=c(1,96))  +ggtitle(expression(rho == 0.289))




B  =merge(rbindlist(lapply(mapping_file[protocol == 'simple_screening' & monoculture == FALSE]$file,fread)),mapping_file)
B[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
B[Transfer ==1]
B_Plot = B[Transfer ==1]
B_Plot$offspring_rank = B[Transfer ==40]$rank
print(cor.test(B_Plot[seed==2]$rank,B_Plot[seed==2]$offspring_rank,method='spearman'))
B_Plot[,Spearmans:=cor(rank,offspring_rank,method='spearman'),by=list(seed)]
B_Plot = B_Plot[Well == 'W0']
print(paste('For Seed 2 spearmans is ',B_Plot[seed==2]$Spearmans[1]))

p3 <- ggplot(B_Plot,aes(x= Spearmans)) + geom_histogram(fill='#E69F00',col='Black',binwidth=0.01)  +theme_pubr() +
  labs(x = expression(rho),y='Number of NS Experiments') + geom_vline(xintercept = B_Plot[seed==2]$Spearmans[1],linetype=2,col='red') 

C =merge(rbindlist(lapply(mapping_file[protocol == 'simple_screening' & monoculture == FALSE]$file,fread)),mapping_file)
C1 =merge(rbindlist(lapply(mapping_file[protocol == 'pool_top25' & monoculture == FALSE  & is.na(bottleneck_size)]$file,fread)),mapping_file)
C2 =merge(rbindlist(lapply(mapping_file[protocol == 'Mueller2019' & monoculture == FALSE]$file,fread)),mapping_file)
C3 =merge(rbindlist(lapply(mapping_file[protocol == 'select_top25' & monoculture == FALSE & is.na(bottleneck_size)]$file,fread)),mapping_file)
C4 =merge(rbindlist(lapply(mapping_file[protocol == 'Swenson2000b' & monoculture == FALSE]$file,fread)),mapping_file)
C[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
C1[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
C2[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
C3[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
C4[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
C = C[Well=='W1' & Transfer==40]
C1 = C1[Well=='W1' & Transfer==40]
C2 = C2[Well=='W1' & Transfer==40]
C3 = C3[Well=='W1' & Transfer==40]
C4 = C4[Well=='W1' & Transfer==40]
C1$Q =C1$maximum-C$maximum
C1$Method = 'Migrant Pool'
C1$Selection = 'Stable'

C2$Q =C2$maximum-C$maximum
C2$Method = 'Migrant Pool'
C2$Selection = 'Unstable'

C3$Q =C3$maximum-C$maximum
C3$Method = 'Propagule'
C3$Selection = 'Stable'

C4$Q =C4$maximum-C$maximum
C4$Method = 'Propagule'
C4$Selection = 'Unstable'

C = rbind(C1,C2,C3,C4)
p4 <- ggboxplot(C[Method =='Propagule'],x='Selection',y='Q', add = "jitter",legend='right',shape=1,outlier.size=1,outlier.colour='white',col='Purple') + 
  stat_compare_means(paired=TRUE,method='t.test',label.y =100, comparison = list(c('Unstable','Stable')),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))) +
  scale_y_continuous(limits=c(-1500,500),breaks=c(-1500,500)) + 
  theme(axis.title=element_text(size=12),axis.text.x=element_text(angle=-45)) + labs(x='')+ ggtitle('Propagule') +  
  theme(plot.title = element_text(size = 10,colour = 'Purple'))


p5 <- ggboxplot(C[Method =='Migrant Pool'],x='Selection',y='Q', add = "jitter",legend='right',shape=1,outlier.size=1,outlier.colour='white',col='Orange') + 
  stat_compare_means(paired=TRUE,method='t.test',label.y = 300, comparison = list(c('Unstable','Stable')),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))) +
  scale_y_continuous(limits=c(-1500,500),breaks=c(-1500,500)) + 
  theme(axis.title=element_text(size=12),axis.text.x=element_text(angle=-45)) + labs(x='')+ ggtitle('Migrant Pool') +  
  theme(plot.title = element_text(size = 10,colour = 'Orange'))


D2 = ggarrange(p4,p5,labels=c('D','E'))


ggsave('../Plots/FigS6.png',ggarrange(p1,p2,p3,D2,labels= c('A','B','C',''),heights=c(2,1.75)),width=8,height=7)