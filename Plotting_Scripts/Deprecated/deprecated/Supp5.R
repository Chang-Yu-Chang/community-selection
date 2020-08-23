library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive' & Assembly %in% c("ctrl","simple_screening","coalescence",
                                                                                    "bottleneck",
                                                                                    "knock_in",
                                                                                    "knock_out",
                                                                                    "migration",
                                                                                    "resource_add")]
function_df$Pool_ID = paste(function_df$CommunityPhenotypeName,function_df$SpeciesPool)
function_df$Run_ID = paste(function_df$CommunityPhenotypeName,function_df$SpeciesPool,function_df$Assembly)

ctrl2 = function_df[function_df$Assembly == 'simple_screening']
function_df = function_df[SpeciesPool %in% ctrl2$SpeciesPool,]
ctrl1 = function_df[function_df$Assembly == 'ctrl']

#Simple_Screen
max_ss1=c()
max_ss2 =c()
max_ss3 =c()
max_ss4 =c()

for(j in unique(ctrl1$Pool_ID)){
  tdf1 =ctrl1[Pool_ID == j & Transfer == 40,]
  tdf2 =ctrl2[Pool_ID == j & Transfer == 40,]
  tdf3 =ctrl1[Pool_ID == j & Transfer == 20,]
  best_well = tdf3[CommunityPhenotype == max(CommunityPhenotype)]$Well
  max_ss1 = c(max_ss1,max(tdf1$CommunityPhenotype))
  max_ss2 = c(max_ss2,max(tdf2$CommunityPhenotype))
  max_ss3 = c(max_ss3,max(tdf3$CommunityPhenotype))
  max_ss4 = c(max_ss4,tdf2[Well==best_well]$CommunityPhenotype)
}

sdf = data.table('Pool_ID' = unique(function_df$Pool_ID),
                 'Ctrl' = max_ss1,
                 'Screen' = max_ss2,
                 'T20'  = max_ss3,
                 'Kept'  = max_ss4)
sdf$CommunityPhenotypeName = sapply(sdf$Pool_ID,function(x) strsplit(as.character(x),split=' ')[[1]][1])
sdf$SpeciesPool = sapply(sdf$Pool_ID,function(x) strsplit(as.character(x),split=' ')[[1]][2])
Full_Stat_df = data.table()

for(k in unique(sdf$Pool_ID)){
  tfdf = function_df[Pool_ID == k & Transfer ==40,]
  tfdf$FitnessCtrl = tfdf$CommunityPhenotype/sdf[Pool_ID ==k]$Ctrl
  tfdf$FitnessScreen = tfdf$CommunityPhenotype/sdf[Pool_ID ==k]$Screen
  tfdf$FitnessT20 = tfdf$CommunityPhenotype/sdf[Pool_ID ==k]$T20
  tfdf$FitnessKept = tfdf$CommunityPhenotype/sdf[Pool_ID ==k]$Kept
  Stat_df1 = tfdf[,max(FitnessCtrl), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]
  Stat_df2 = tfdf[,max(FitnessScreen), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]
  Stat_df3 = tfdf[,max(FitnessT20), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]
  Stat_df4 = tfdf[,max(FitnessKept), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]
  
  colnames(Stat_df1)[ncol(Stat_df1)]  = 'MaxCtrl'
  Stat_df1$MaxScreen = Stat_df2$V1
  Stat_df1$MaxT20 = Stat_df3$V1
  Stat_df1$MaxKept= Stat_df4$V1
  Full_Stat_df = rbind(Full_Stat_df,Stat_df1)
}
Pertubations = Full_Stat_df[Assembly %in% c("coalescence",
                                            "bottleneck",
                                            "knock_in",
                                            "knock_out",
                                            "migration",
                                            "resource_add")]
Pertubations$Assembly  = factor(Pertubations$Assembly,levels=c('knock_in','knock_out','bottleneck', 'coalescence','migration','resource_add'))
p1 <- ggplot(Pertubations,
             aes(x=Assembly,y=MaxCtrl,col=Assembly)) + 
              geom_boxplot(outlier.shape = NA) +  
              geom_jitter(height=0,size=1)  + 
              theme_classic() + scale_y_log10() +
              scale_y_continuous(limits=c(0,2)) + 
              geom_abline(intercept=1,slope=0,linetype=2,col='red') +
              scale_x_discrete(labels = c('Knock In','Knock Out','Bottleneck','Coalescence' ,'Migration',' Resource Shift')) +
              labs(x = 'Type of Pertubation', y =  expression(F[max*(Protocol)]/F[max*(NoPertubationCtrl)])) +  guides(col =FALSE) +
              scale_colour_manual(values = c('Orange','Orange','Orange','Orange','Orange','Orange'))
p2 <- ggplot(Pertubations,
             aes(x=Assembly,y=MaxScreen,col=Assembly)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  theme_classic() + scale_y_log10() +
  scale_y_continuous(limits=c(0,2)) + 
  geom_abline(intercept=1,slope=0,linetype=2,col='red') +
  scale_x_discrete(labels = c('Knock In','Knock Out','Bottleneck','Coalescence' ,'Migration',' Resource Shift')) +
  labs(x = 'Type of Pertubation', y =  expression(F[max*(Protocol)]/F[max*(Screen)])) +  guides(col =FALSE) +
  scale_colour_manual(values = c('Orange','Orange','Orange','Orange','Orange','Orange'))
p3 <- ggplot(Pertubations,
             aes(x=Assembly,y=MaxT20,col=Assembly)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  theme_classic() + scale_y_log10() +
  scale_y_continuous(limits=c(0,2)) + 
  geom_abline(intercept=1,slope=0,linetype=2,col='red') +
  scale_x_discrete(labels = c('Knock In','Knock Out','Bottleneck','Coalescence' ,'Migration',' Resource Shift')) +
  labs(x = 'Type of Pertubation', y =  expression(F[max*(Protocol)]/F[max*(T20)])) +  guides(col =FALSE) +
  scale_colour_manual(values = c('Orange','Orange','Orange','Orange','Orange','Orange'))
p4 <- ggplot(Pertubations,
             aes(x=Assembly,y=MaxKept,col=Assembly)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  theme_classic() + scale_y_log10() +
  scale_y_continuous(limits=c(0,2)) + 
  geom_abline(intercept=1,slope=0,linetype=2,col='red') +
  scale_x_discrete(labels = c('Knock In','Knock Out','Bottleneck','Coalescence' ,'Migration',' Resource Shift')) +
  labs(x = 'Type of Pertubation', y =  expression(F[max*(Protocol)]/F[(Unpeturbed)])) +  guides(col =FALSE) +
  scale_colour_manual(values = c('Orange','Orange','Orange','Orange','Orange','Orange'))
ggsave('../figure/Supp5.png',ggarrange(p1,p2,p3,p4,labels='AUTO'),height=8,width=10)
