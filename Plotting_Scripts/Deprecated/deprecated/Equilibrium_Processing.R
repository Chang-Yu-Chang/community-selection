library(data.table)
library(ggplot2)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df$Pool_ID = paste(function_df$CommunityPhenotypeName,function_df$SpeciesPool)
ctrl = function_df[function_df$Assembly == 'ctrl']

#Simple_Screen
max_ss =c()
max_ss2 =c()
cov_ss =c()
for(j in unique(ctrl$Pool_ID)){
  tdf =ctrl[Pool_ID == j & Transfer == 40,]
  tdf2 =ctrl[Pool_ID == j & Transfer == 20,]
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
  tfdf$Fitness = tfdf$CommunityPhenotype/sdf[Pool_ID ==k]$Maximum
  Stat_df = tfdf[,max(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]
  colnames(Stat_df)[ncol(Stat_df)]  = 'Max'
  Stat_df$Variance =  tfdf[,var(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]$V1
  Stat_df$Mean =  tfdf[,mean(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]$V1
  Stat_df$Fben =  tfdf[,sum(Fitness>1.01), by = .(Assembly,CommunityPhenotypeName,SpeciesPool,Pool_ID)]$V1
  Full_Stat_df = rbind(Full_Stat_df,Stat_df)
}
Pertubations = Full_Stat_df[Assembly %in% c("bottleneck",
                                            "coalescence",
                                            "knock_in_isolates",
                                            "knock_in",
                                            "knock_out",
                                            "migration",
                                            "resource") & CommunityPhenotypeName == 'f1_additive']
p1 <- ggplot(Pertubations,
             aes(x=Assembly,y=Max)) + geom_point() + geom_boxplot() + scale_y_continuous(limits=c)

p2 <- ggplot(function_df[SpeciesPool==13 & Assembly =='knock_in' & CommunityPhenotypeName == 'f1_additive'],
             aes(x=Transfer,y=CommunityPhenotype,group=Well)) + geom_point() +geom_abline()