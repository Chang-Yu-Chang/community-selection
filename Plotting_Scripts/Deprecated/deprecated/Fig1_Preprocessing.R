library(data.table)
library(ggplot2)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df$Pool_ID = paste(function_df$CommunityPhenotypeName,function_df$SpeciesPool)
end_function = function_df[Transfer==40 & CommunityPhenotypeName == 'f1_additive']

ctrl = end_function[end_function$Assembly == 'ctrl']
#Simple_Screen
max_ss =c()
cov_ss =c()
for(j in unique(ctrl$Pool_ID)){
  tdf =ctrl[Pool_ID == j & Transfer == 40,]
  max_ss = c(max_ss,max(tdf$CommunityPhenotype))
}

sdf = data.frame('Pool_ID' = unique(ctrl$Pool_ID),'Maximum' = max_ss)
sdf$Species_pool = sapply(sdf$Pool_ID,function(x) strsplit(as.character(x),split= ' ')[[1]][2])
end_function$Pool_Algorithm = paste(end_function$SpeciesPool,end_function$Assembly)
performance =c()
for(k in unique(end_function$Pool_Algorithm)){
  tdf = end_function[Pool_Algorithm == k]
  performance = c(performance,max(tdf$CommunityPhenotype)/sdf[sdf$Species_pool == tdf$SpeciesPool[1],]$Maximum)
}
  

sdf2 = data.frame(Pool_Algorithm = unique(end_function$Pool_Algorithm),'Performance' = performance)
p1 <-  ggplot(sdf2,aes(x=Assembly,y=Performance))