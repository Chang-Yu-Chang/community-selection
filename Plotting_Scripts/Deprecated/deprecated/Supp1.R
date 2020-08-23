library(data.table)
library(ggplot2)
library(ggpubr)
library(operators)
library(gridExtra)
library(RColorBrewer)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive' & Assembly %in% c("Arora2019_V2",
                                                                                    "Arora2019_V2_control",
                                                                                    "Panke_Buisse2015",
                                                                                    "Blouin2015",
                                                                                    "Blouin2015_control",
                                                                                    "Mueller2019",
                                                                                    "Swenson2000a",
                                                                                    "Swenson2000a_control",
                                                                                    "Swenson2000b",
                                                                                    "Swenson2000b_control",
                                                                                    "Wright2019",
                                                                                    "Wright2019_control",
                                                                                    "Raynaud2019a_V2",
                                                                                    "Raynaud2019a_V2_control",
                                                                                    "Raynaud2019b_V2",
                                                                                    "Raynaud2019b_V2_control",
                                                                                    "Jochum2019",
                                                                                    "monoculture","ctrl","coalescence",
                                                                                    "pool_top25","select_top25"),]

function_df[Assembly =='Raynaud2019a_V2']$Assembly = 'Raynaud2019a'
function_df[Assembly =='Raynaud2019b_V2']$Assembly = 'Raynaud2019b'
function_df[Assembly =='Raynaud2019a_V2_control']$Assembly = 'Raynaud2019a_control'
function_df[Assembly =='Raynaud2019b_V2_control']$Assembly = 'Raynaud2019b_control'
function_df[Assembly =='Arora2019_V2']$Assembly = 'Arora2019'
function_df[Assembly =='Arora2019_V2_control']$Assembly = 'Arora2019_control'

ctrl = function_df[function_df$Assembly == 'monoculture' & Transfer ==40]
ctrl_max = ctrl[,max(CommunityPhenotype),by=.(Assembly,CommunityPhenotypeName,SpeciesPool)]
function_df = function_df[SpeciesPool %in% ctrl_max$SpeciesPool]
equilibrium = function_df[Transfer==40,]
equilibrium$Fitness = 0

for(i in unique(ctrl$SpeciesPool)){
  equilibrium[SpeciesPool==i,]$Fitness = equilibrium[SpeciesPool==i,]$CommunityPhenotype/ctrl_max[SpeciesPool==i,]$V1
  
}
Performance =  equilibrium[,max(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool)]
colnames(Performance)[4]  = 'Performance'
order_assembly =c('ctrl','monoculture','Swenson2000a',
                  'Blouin2015',
                  'Panke_Buisse2015',
                  'Jochum2019',
                  'Raynaud2019b',
                  'Mueller2019',
                  'Wright2019',
                  'Swenson2000b','Arora2019','Raynaud2019a','coalescence')
Performance = Performance[Assembly %in% order_assembly]
Performance$Assembly = factor(Performance$Assembly,
                              levels=order_assembly)
Performance$Method = 'Pooling'
Performance[Assembly %in% c('Swenson2000b','Arora2019','Raynaud2019a')]$Method = 'Single Parent'
Performance[Assembly=='coalescence']$Method = 'Directed Selection'
Performance[Assembly == 'coalescence']$Assembly = 'Directed Selection'
p1 <- ggplot(Performance[Assembly != 'ctrl' & Assembly != 'monoculture' & Assembly != 'Directed Selection'],
             aes(x=Assembly,y=Performance,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0)  + 
  scale_y_continuous(breaks=c(-1,0,1,2),limits=c(-1,2)) +   
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90)) + 
  scale_shape_manual(values=c(3,1,2)) + 
  scale_colour_manual(values = c('Orange','Purple')) + 
  labs(x = '',y=expression(F[max*(Protocol)]/F[max*(Monoculture)]))

ggsave('../figure/Supp1.png',p1)