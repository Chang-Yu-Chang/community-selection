rm(list=ls())
library(data.table)
library(ggplot2)
library(vegan)
library(MASS)
library(fitdistrplus)
library(ggpubr)

plate_power <- t(round(fread("../Data/Rarefaction_Data/plate_power.txt")[,1:11]*1e+6))
plate_lognormal <- t(round(fread("../Data/Rarefaction_Data/plate_lognormal.txt")[,1:11]*1e+6))
plate_default <- t(round(fread("../Data/Rarefaction_Data/plate_default.txt")[,1:11]*1e+6))
samples = fread('../Data/Rarefaction_Data/metadata.csv')
samples = samples[Transfer==0]
otu_table = fread('../Data/Rarefaction_Data/otu_table.csv')
otu_table = t(as.matrix(otu_table[,which(colnames(otu_table) %in% samples$ID),with=FALSE]))
otu_table = otu_table[,colSums(otu_table) >0]
sample_sizes =round(10^seq(1,6,by=0.1))
experiment_curve = sapply(sample_sizes,function(x) as.vector(rarefy(otu_table,x)))
default_curve = sapply(sample_sizes,function(x) as.vector(rarefy(plate_default,x)))
lognormal_curve = sapply(sample_sizes,function(x) as.vector(rarefy(plate_lognormal,x)))
power_curve = sapply(sample_sizes,function(x) as.vector(rarefy(plate_power,x)))

plot_df = data.frame(N = rep(sample_sizes,4),
          Treatment = rep(c('Experimental Data','Uniform Abundance','Power Distribution','Lognormal Distribution'),each=length(sample_sizes)) )
plot_df = cbind(plot_df,rbind(t(experiment_curve),t(default_curve),t(power_curve),t(lognormal_curve)))
plot_df = melt(plot_df, id.vars=c("N", "Treatment"),value.name='Richness',variable.name = 'Community')

plot_df$Richness = as.numeric(plot_df$Richness)
p1 <- ggplot(plot_df,aes(x=N,y=Richness,col=Community)) + geom_line() + facet_wrap(~Treatment) + guides(col=FALSE) +
  theme_pubr() + scale_y_log10() + scale_x_log10() + labs(x = 'Number of Individuals', y = 'Number of Species')
ggsave('../Plots/FigS11.png',p1)