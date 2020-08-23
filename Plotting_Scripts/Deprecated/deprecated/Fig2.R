library(data.table)
library(ggplot2)
library(ggpubr)
library(gridExtra)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive' & Assembly %in% c("ctrl","simple_screening","coalescence",
                                                                                    "bottleneck",
                                                                                    "knock_in",
                                                                                    "knock_out",
                                                                                    "migration",
                                                                                    "resource_add")]
function_df$Pool_ID = paste(function_df$CommunityPhenotypeName,function_df$SpeciesPool)
ctrl = function_df[function_df$Assembly == 'simple_screening']

#Simple_Screen
max_ss =c()
max_ss2 =c()
cov_ss =c()
for(j in unique(ctrl$Pool_ID)){
  tdf =ctrl[Pool_ID == j & Transfer == 40,]
  tdf2 =ctrl[Pool_ID == j & Transfer == 40,]
  max_ss = c(max_ss,max(tdf$CommunityPhenotype))
  max_ss2 = c(max_ss2,max(tdf2$CommunityPhenotype))
  cov_ss = c(cov_ss,cov(tdf$CommunityPhenotype,tdf$Richness)/var(tdf$Richness))
}

sdf = data.table('Pool_ID' = unique(ctrl$Pool_ID),'Maximum' = max_ss,'Maximum_20' = max_ss)
sdf$CommunityPhenotypeName = sapply(sdf$Pool_ID,function(x) strsplit(as.character(x),split=' ')[[1]][1])
sdf$SpeciesPool = sapply(sdf$Pool_ID,function(x) strsplit(as.character(x),split=' ')[[1]][2])
Full_Stat_df = data.table()

for(k in unique(sdf$Pool_ID)){
  tfdf = function_df[Pool_ID == k & Transfer ==40,]
  tfdf2 = function_df[Pool_ID == k & Transfer ==20,]
  tfdf$Fitness = tfdf$CommunityPhenotype/sdf[Pool_ID ==k]$Maximum
  Stat_df = tfdf[,max(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]
  colnames(Stat_df)[ncol(Stat_df)]  = 'Max'
  Stat_df$Variance =  tfdf[,var(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]$V1
  Stat_df$Mean =  tfdf[,mean(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]$V1
  Stat_df$Fben =  tfdf[,sum(Fitness>1.01), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]$V1
  Full_Stat_df = rbind(Full_Stat_df,Stat_df)
}
Pertubations = Full_Stat_df[Assembly %in% c("coalescence",
                                            "bottleneck",
                                            "knock_in",
                                            "knock_out",
                                            "migration",
                                            "resource_add")]
Pertubations$Assembly  = factor(Pertubations$Assembly,levels=c('knock_in','knock_out','bottleneck', 'coalescence','migration','resource_add'))
p7 <- ggplot(Pertubations[Assembly != 'ctrl' 
                          & Assembly != 'simple_screening' 
                          & Assembly != 'Directed Selection'],
             aes(x=Assembly,y=Max,group=Assembly)) + 
  geom_boxplot(outlier.shape = NA,col='Orange') +  
  geom_jitter(height=0,col='Orange',shape=3)  + 
  scale_y_continuous(breaks=c(0,1,2),limits=c(0,2)) +   
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90)) + 
  labs(x = '',y=expression(F[max*(Protocol)]/F[max*(Screen)])) +
  scale_x_discrete(labels = c('Knock In','Knock Out','Bottleneck','Coalescence' ,'Migration',' Resource Shift'))  + scale_y_log10()

A = function_df[Assembly == 'knock_out' & SpeciesPool==1]
maxt20 = max(A[Transfer==20]$CommunityPhenotype)
A[Transfer>20]$Well = paste(A[Transfer>20]$Well,'_Rep')
temp_A = A[Transfer ==21]
temp_A$CommunityPhenotype= maxt20
temp_A$Transfer =20
A = rbind(A,temp_A)
p1 <- ggplot(A,aes(x=Transfer,y=CommunityPhenotype,group=Well)) + 
  geom_line(col='Grey') +  
  theme_pubr() +labs(y='',x='')

B = function_df[Assembly == 'knock_in' & SpeciesPool==1]
maxt20 = max(B[Transfer==20]$CommunityPhenotype)
B[Transfer>20]$Well = paste(B[Transfer>20]$Well,'_Rep')
temp_B = B[Transfer ==21]
temp_B$CommunityPhenotype= maxt20
temp_B$Transfer =20
B = rbind(B,temp_B)
p2 <- ggplot(B,aes(x=Transfer,y=CommunityPhenotype,group=Well)) + 
  geom_line(col='Grey') +  
  theme_pubr() +labs(y='',x='')

C = function_df[Assembly == 'bottleneck' & SpeciesPool==1]
maxt20 = max(C[Transfer==20]$CommunityPhenotype)
C[Transfer>20]$Well = paste(C[Transfer>20]$Well,'_Rep')
temp_C = C[Transfer ==21]
temp_C$CommunityPhenotype= maxt20
temp_C$Transfer =20
C = rbind(C,temp_C)
p3 <- ggplot(C,aes(x=Transfer,y=CommunityPhenotype,group=Well)) + 
  geom_line(col='Grey') +  
  theme_pubr() +labs(y='',x='')

D = function_df[Assembly == 'migration' & SpeciesPool==1]
maxt20 = max(D[Transfer==20]$CommunityPhenotype)
D[Transfer>20]$Well = paste(D[Transfer>20]$Well,'_Rep')
temp_D = D[Transfer ==21]
temp_D$CommunityPhenotype= maxt20
temp_D$Transfer =20
D = rbind(D,temp_D)
p4 <- ggplot(D,aes(x=Transfer,y=CommunityPhenotype,group=Well)) + 
  geom_line(col='Grey') +  
  theme_pubr() +labs(y='',x='')

E = function_df[Assembly == 'coalescence' & SpeciesPool==1]
maxt20 = max(E[Transfer==20]$CommunityPhenotype)
E[Transfer>20]$Well = paste(E[Transfer>20]$Well,'_Rep')
temp_E = E[Transfer ==21]
temp_E$CommunityPhenotype= maxt20
temp_E$Transfer =20
E = rbind(E,temp_E)
p5 <- ggplot(E,aes(x=Transfer,y=CommunityPhenotype,group=Well)) + 
  geom_line(col='Grey') +  
  theme_pubr() +labs(y='',x='')

F = function_df[Assembly == 'resource_add' & SpeciesPool==1]
maxt20 = max(F[Transfer==20]$CommunityPhenotype)
F[Transfer>20]$Well = paste(F[Transfer>20]$Well,'_Rep')
temp_F = F[Transfer ==21]
temp_F$CommunityPhenotype= maxt20
temp_F$Transfer =20
F = rbind(F,temp_F)
p6 <- ggplot(F,aes(x=Transfer,y=CommunityPhenotype,group=Well)) + 
  geom_line(col='Grey') +  
  theme_pubr() +labs(y='',x='')
pA<- ggarrange(p1,p2,p3,p4,p5,p6,nrow=1,labels='AUTO')

ggsave('../figure/Fig2.png',
       grid.arrange(pA,
                    ggarrange(p7,labels=c('G'),ncol=1)),width=15,height=7)
