rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)
library(png)
library(grid)

normalize <- function(x){
  return((x/sum(x)))
}
comp = fread('../data/f1_additive-simple_screening-1-internal_composition.csv')
comp = comp[Well == 'W0' & Type == 'consumer',]
# comp$Transfer = paste('Generation', comp$Transfer,sep =' ')
p1<- ggplot(comp[Transfer %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)],aes(x=Time,y=Abundance,fill=as.factor(ID),col=as.factor(ID),width=0.85)) + 
  geom_bar(stat='identity',position="fill",col='grey20',size=0.4) +
  theme_pubr() + guides(fill=FALSE,col=FALSE)+
  theme(legend.position = "right")  + 
  # scale_y_continuous(breaks=c(0,0.5,1))  +
  labs(x='Time',y = 'Relative Abundance',fill='')   + 
  facet_wrap(~Transfer,nrow=2) + scale_y_continuous(breaks=c(0,1)) +scale_x_continuous(breaks = c(0,9),labels = c(0,'t'))

ggsave('../Plots/FigS3.png',p1,width=12,height=6)