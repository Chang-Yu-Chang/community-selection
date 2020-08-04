rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)
library(png)
library(grid)
library(cowplot)
k = 3

mapping_file = fread('../Data/Mapping_Files/input_iteration.csv')
mapping_file$file = paste('../Data/Raw/',mapping_file$exp_id,'_function.txt',sep='')
t_bottleneck = mapping_file[grep('iteration_1',mapping_file$exp_id),]
t_bottleneck = t_bottleneck[grep('round2',t_bottleneck$exp_id),]
t_bottleneck = merge(rbindlist(lapply(t_bottleneck$file,fread)),t_bottleneck)


t_bottleneck[Transfer == 20]$Biomass *t_bottleneck[Transfer == 20]$scale * t_bottleneck[Transfer == 20]$dilution * t_bottleneck[Transfer==20]$bottleneck_size
t = mapping_file[seed ==k]
t_bottleneck = t[grep('iteration_1',t$exp_id),]
t_bottleneck$starting_n =t_bottleneck$n_transfer * seq(0,nrow(t_bottleneck)-1)
t_migration = t[grep('iteration_6',t$exp_id),]
t_migration$starting_n =t_migration$n_transfer * seq(0,nrow(t_migration)-1)
t_Both = t[grep('iteration_5',t$exp_id),]
t_Both$starting_n =t_Both$n_transfer * seq(0,nrow(t_Both)-1)

t_bottleneck = merge(rbindlist(lapply(t_bottleneck$file,fread)),t_bottleneck)
t_bottleneck = t_bottleneck[Transfer!=0]
t_bottleneck$Transfer2 = t_bottleneck$starting_n + t_bottleneck$Transfer
t_migration = merge(rbindlist(lapply(t_migration$file,fread)),t_migration)
t_migration = t_migration[Transfer!=0]
t_migration$Transfer2 = t_migration$starting_n + t_migration$Transfer
t_Both = merge(rbindlist(lapply(t_Both$file,fread)),t_Both)
t_Both = t_Both[Transfer!=0]
t_Both$Transfer2 = t_Both$starting_n + t_Both$Transfer

t_bottleneck$Treatment = 'Bottleneck'
t_migration$Treatment = 'Migration'
t_Both$Treatment = 'Both'
t = rbind(t_bottleneck,t_migration,t_Both)
t$Treatment = factor(t$Treatment,levels=c('Bottleneck','Migration','Both'))
t$Highlight = FALSE
screen_max = max(t[Transfer2 == 30]$CommunityPhenotype)
df_vline = data.frame(Xinter = rep(seq(30,430,by=20),3),
                      Treatment=rep(c('Bottleneck','Migration','Both'),each=21))
t$Well2 = t$Well
# for(i in 1:nrow(df_vline)){
#   temp_t  = t[Transfer2 == df_vline[,1][i] & Treatment == df_vline[,2][i]]
#   temp_well = temp_t[CommunityPhenotype == max(CommunityPhenotype)]$Well
#   temp_n = temp_t$starting_n[1]
#   t[Transfer2 <= df_vline[,1][i] & Transfer2 > df_vline[,1][i]-temp_t$n_transfer[1] &  
#       Well == temp_well & Treatment == df_vline[,2][i]]$Highlight=TRUE
#   t[Transfer2 <= df_vline[,1][i] & Transfer2 > df_vline[,1][i]-temp_t$n_transfer[1] &  
#      Treatment == df_vline[,2][i]]$Well2=paste(t[Transfer2 <= df_vline[,1][i] & Transfer2 > df_vline[,1][i]-temp_t$n_transfer[1] &  
#                                                    Treatment == df_vline[,2][i]]$Well2 ,temp_n)
#   
# }
t[,Top := CommunityPhenotype==max(CommunityPhenotype),by=list(Treatment,Transfer2)]
img1 =  readPNG( "../Plots/Cartoons/Fig3A.png", TRUE)

p1 <- rasterGrob(img1, interpolate=TRUE)
p2 <- ggplot() + 
  geom_line(t[Treatment == 'Bottleneck'],mapping = aes(x= Transfer2,y=CommunityPhenotype, group = Well),col='Gray90',size=0.2) +
  geom_line(t[Top==TRUE & Treatment == 'Bottleneck'],mapping = aes(x= Transfer2,y=CommunityPhenotype),col='Gray20',size=0.4) +
  theme_pubr()  + labs(y='F',x='Generation') +   
  theme(legend.position = "top", legend.title = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        strip.background = element_blank(), strip.text = element_blank()) +
  panel_border(color = 1, size = 1)  +
  geom_hline(yintercept = screen_max,col='Red',linetype=2) +
  geom_vline(data = df_vline, aes(xintercept = Xinter),col = '#D95F02', size = 0.5,linetype=2) +
  scale_y_continuous(breaks=c(-1000,1000),limits=c(-1250,1250)) + guides(col=FALSE) +
  # scale_colour_brewer(palette='Dark2') +
  scale_x_continuous(expand=c(0,0),breaks = c(0,200,400),limits=c(0,460),labels=c('','','')) + 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8),axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0))) +
  ggtitle('Bottleneck')+ 
  theme(plot.title = element_text(size = 10,colour = '#D95F02'))


p3 <- ggplot() + 
  geom_line(t[Treatment == 'Migration'],mapping = aes(x= Transfer2,y=CommunityPhenotype, group = Well),col='Gray90',size=0.2) +
  geom_line(t[Top==TRUE & Treatment == 'Migration'],mapping = aes(x= Transfer2,y=CommunityPhenotype),col='Gray20',size=0.4) +
  theme_pubr()  + labs(y='F',x='Generation') +   
  theme(legend.position = "top", legend.title = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        strip.background = element_blank(), strip.text = element_blank()) +
  panel_border(color = 1, size = 1)  +
  geom_hline(yintercept = screen_max,col='Red',linetype=2) +
  geom_vline(data = df_vline, aes(xintercept = Xinter),col = '#7570B3', size = 0.5,linetype=2) +
  scale_y_continuous(breaks=c(-1000,1000),limits=c(-1250,1250)) + guides(col=FALSE) +
  scale_colour_brewer(palette='Dark2') +
  scale_x_continuous(expand=c(0,0),breaks = c(0,200,400),limits=c(0,460) ,labels=c('','','')) +
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8),axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0))) +
  ggtitle('Migration')+ 
  theme(plot.title = element_text(size = 10,colour='#7570B3'))

p4 <- ggplot() + 
  geom_line(t[Treatment == 'Both'],mapping = aes(x= Transfer2,y=CommunityPhenotype, group = Well),col='Gray90',size=0.2) +
  geom_line(t[Top==TRUE & Treatment == 'Both'],mapping = aes(x= Transfer2,y=CommunityPhenotype),col='Gray20',size=0.4) +
  theme_pubr()  + labs(y='F',x='Generation') +   
  theme(legend.position = "top", legend.title = element_blank(), 
        panel.border = element_blank(), panel.grid = element_blank(),
        strip.background = element_blank(), strip.text = element_blank()) +
  panel_border(color = 1, size = 1)  +
  geom_hline(yintercept = screen_max,col='Red',linetype=2) +
  geom_vline(data = df_vline, aes(xintercept = Xinter),col = '#1B9E77', size = 0.5,linetype=2) +
  scale_y_continuous(breaks=c(-1000,1000),limits=c(-1250,1250)) + guides(col=FALSE) +
  scale_colour_brewer(palette='Dark2') +
  scale_x_continuous(expand=c(0,0),breaks = c(0,200,400),limits=c(0,460))  +
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8),axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0))) +
  ggtitle('Bottleneck + Migration')  + 
  theme(plot.title = element_text(size = 10,colour = '#1B9E77'))

t2 = mapping_file[protocol=='simple_screening']
t2_control = t2[grep('simple_screening',t2$exp_id)]
t2_control = merge(rbindlist(lapply(t2_control$file,fread)),t2_control)
t2_control[,Maximum:=max(CommunityPhenotype),by=list(exp_id,Transfer)]

t2 = mapping_file[protocol=='simple_screening']
t2 = t2[grep('round23',t2$exp_id)]
t2 = t2[!grep('iteration_4',t2$exp_id)]
t2 = merge(rbindlist(lapply(t2$file,fread)),t2)
t2[,Maximum:=max(CommunityPhenotype),by=list(exp_id,Transfer)]
t2$Treatment = 'Other'
t2[grep('iteration_1',t2$exp_id)]$Treatment = 'Bottleneck'
t2[grep('iteration_6',t2$exp_id)]$Treatment = 'Migration'
t2[grep('iteration_5',t2$exp_id)]$Treatment = 'Both'
t2 = t2[Treatment !='Other']
t2$Treatment = factor(t2$Treatment,levels=c('Bottleneck','Migration','Both'))

t2 = t2[Well == 'W0' & Transfer ==20]
t2_control = t2_control[Well == 'W0' & Transfer ==40]
t2$Q = t2$Maximum - rep(t2_control$Maximum,3)
my_comparisons <- list( c("Bottleneck", "Migration"), c("Bottleneck", "Both"), c("Migration", "Both") )

p5 <- ggboxplot(t2,x='Treatment',y='Q',col='Treatment',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white') + 
  stat_compare_means(paired=TRUE,comparisons = my_comparisons,method='t.test',size=3,
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))) + 
  guides(col=FALSE) +   labs(x = '',y = 'Q') + 
  scale_colour_manual(values = c('#D95F02','#7570B3','#1B9E77')) + guides(fill=FALSE)+
  scale_x_discrete(labels =c('Bottleneck','Migration','Bottleneck +\n Migration'))+
  scale_y_continuous(breaks=c(0,1000)) + 
                       theme(axis.title=element_text(size=12),
                             axis.text.x=element_text(angle=-45,hjust=0.3,colour=c('#D95F02','#7570B3','#1B9E77')),
                             axis.title.y = element_text(margin = margin(t = 0, r = -5, b = 0, l = 0))) 

a = ggarrange(p1,labels ='A',font.label = list(size = 12))
b = ggarrange(p2,p3,p4,labels=c('B','C','D'),ncol=1,nrow=3,font.label = list(size = 12),vjust=0)
b2 = annotate_figure(b,bottom=text_grob('Generation'))
c = ggarrange(p5,labels=c('E'),font.label = list(size = 12),vjust=0)
ggsave('../Plots/Fig3.png',ggarrange(a,ggarrange(b2,c,ncol=2,nrow=1,widths=c(2,1)),ncol=1,nrow=2,heights=c(1,1)),height=8,width=8)



