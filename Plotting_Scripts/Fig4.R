rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(operators)
library(png)
library(grid)
library(cowplot)

img1 =  readPNG( "../Plots/Cartoons/Fig4A.png", TRUE)
k = 3
mapping_file = fread('../data/input_additive_robustness_screen.csv')
mapping_file$file = paste('../data/raw/robustness/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file = mapping_file[!(migration == TRUE & is.na(n_migration_ds)) & migration == TRUE,]
t = mapping_file[seed==k]
t = merge(rbindlist(lapply(t$file,fread)),t)
t[,Maximum:=max(CommunityPhenotype),by=list(exp_id,Transfer)]

t_Synthetic = t[grep('iteration_5-3-synthetic_community-migration',t$exp_id)]
t_Synthetic$Community = 'Synthetic'
t_Synthetic = t_Synthetic[Well != t_Synthetic[Transfer==20 & CommunityPhenotype == Maximum]$Well]
t_sel = t[grep('iteration_5-3-selected_community-migration',t$exp_id)]
t_sel$Community = 'DE'
t_sel = t_sel[Well != t_sel[Transfer==20 & CommunityPhenotype == Maximum]$Well]
t_ctrl =  t[grep('simple_screening-3-selected_community-migration',t$exp_id)]
t_ctrl$Community = 'NS'
t_ctrl = t_ctrl[Well != t_ctrl[Transfer==20 & CommunityPhenotype == Maximum]$Well]

t_all = rbind(t_sel,t_Synthetic,t_ctrl)
t_all = t_all[Transfer!=0]
t_all$Well2 = paste(t_all$Well,t_all$Community)
t_all$Community = factor(t_all$Community,levels=c('NS','DE','Synthetic'))
t20_a = t_all[Well == 'W1' & Transfer ==20 & Community == 'Synthetic',]$CommunityPhenotype
t20_b = t_all[Well == 'W1' & Transfer ==40 & Community == 'Synthetic',]$CommunityPhenotype


t_final = t_all[Transfer==40]
comparisons <- list( c("NS", "DE"), c("DE", "Synthetic"), c("NS", "Synthetic") )
means = c(mean(t_final[Community == "NS"]$CommunityPhenotype),mean(t_final[Community == "DE"]$CommunityPhenotype),mean(t_final[Community == 'Synthetic']$CommunityPhenotype))
sds = c(sd(t_final[Community == "NS"]$CommunityPhenotype),sd(t_final[Community == "DE"]$CommunityPhenotype),sd(t_final[Community == 'Synthetic']$CommunityPhenotype))

lines = data.frame(X = c("NS","DE",'Synthetic'),Y = means,Xend = c(2.7,2.7,2.7),Yend=rep(1100,3),Group = c("NS","DE",'Synthetic'))

t_20 = t_all[Transfer==20]
t_final$RS = 1- (2*abs(t_20$CommunityPhenotype-t_final$CommunityPhenotype)/(t_20$CommunityPhenotype+abs(t_20$CommunityPhenotype-t_final$CommunityPhenotype)))
means2 = c(mean(t_final[Community == "NS"]$RS),mean(t_final[Community == "DE"]$RS),mean(t_final[Community == 'Synthetic']$RS))
sds2 = c(sd(t_final[Community == "NS"]$RS),sd(t_final[Community == "DE"]$RS),sd(t_final[Community == 'Synthetic']$RS))

lines2 = data.frame(X = c("NS","DE",'Synthetic'),Y = means2,Xend = c(2.7,2.7,2.7),Yend=rep(0.8,3),Group = c("NS","DE",'Synthetic'))

t_Synthetic_all = mapping_file[grep('iteration_5.*synthetic_community-migration',mapping_file$exp_id)]
t_Synthetic_all$Community = 'Synthetic'
t_sel_all = mapping_file[grep('iteration_5.*selected_community-migration',mapping_file$exp_id)]
t_sel_all$Community = "DE"
t_ctrl_all = mapping_file[grep('simple_screening.*selected_community-migration',mapping_file$exp_id)]
t_ctrl_all$Community = "NS"
t_all2 = rbind(t_sel_all,t_Synthetic_all,t_ctrl_all)
t_all2 = merge(rbindlist(lapply(t_all2$file,fread)),t_all2)
t_all2$Community = factor(t_all2$Community,levels=c("NS","DE",'Synthetic'))

t_final2 = t_all2[Transfer==40]
t20_2 = t_all2[Transfer==20]
t20_2[,Fmax :=max(CommunityPhenotype),by=list(exp_id,Transfer)]

t_final2$RS = 1- (2*abs(t20_2$CommunityPhenotype-t_final2$CommunityPhenotype)/(t20_2$CommunityPhenotype+abs(t20_2$CommunityPhenotype-t_final2$CommunityPhenotype)))
t_final2[,Fmax :=max(CommunityPhenotype),by=list(exp_id,Transfer)]
t_final2[,R :=mean(RS),by=list(exp_id,Transfer)]
t_final2[,Fmean :=mean(CommunityPhenotype),by=list(exp_id,Transfer)]
t_final2$Fmax2 = t20_2$Fmax
t_final2 = t_final2[Well=='W0']

means1 = c(mean(t_final2[Community == "NS"]$Fmax2),mean(t_final2[Community == "DE"]$Fmax2),mean(t_final2[Community == 'Synthetic']$Fmax2))
sds1 = c(sd(t_final2[Community == "NS"]$Fmax2),sd(t_final2[Community == "DE"]$Fmax2),sd(t_final2[Community == 'Synthetic']$Fmax2))
means2 = c(mean(t_final2[Community == "NS"]$Fmean),mean(t_final2[Community == "DE"]$Fmean),mean(t_final2[Community == 'Synthetic']$Fmean))
sds2 = c(sd(t_final2[Community == "NS"]$Fmean),sd(t_final2[Community == "DE"]$Fmean),sd(t_final2[Community == 'Synthetic']$Fmean))
means3 = c(mean(t_final2[Community == "NS"]$R),mean(t_final2[Community == "DE"]$R),mean(t_final2[Community == 'Synthetic']$R))
sds3 = c(sd(t_final2[Community == "NS"]$R),sd(t_final2[Community == "DE"]$R),sd(t_final2[Community == 'Synthetic']$R))



p1 <- rasterGrob(img1,interpolate=TRUE)

p2_inset <- rasterGrob(readPNG( "../Plots/Cartoons/Fig4B.png", TRUE), interpolate=TRUE)

p2 <- ggplot() + 
  geom_line(t_all[Well == 'W1'],mapping = aes(x=Transfer,y=CommunityPhenotype,col=Community,group=Well2),size=1) +
  scale_colour_brewer(palette='Dark2',labels=c("NS","DE",'Synthetic')) +
  geom_line(t_all[Well != 'W1'],mapping = aes(x=Transfer,y=CommunityPhenotype,col=Community,group=Well2),size=1,alpha=0.05)+
  theme_pubr() + labs(x= 'Generation' ,y = 'F',col='') + scale_y_continuous(breaks = c(-500,2000),limits=c(-600,2500)) + 
  scale_x_continuous(breaks = c(0,20,40),limits=c(0,46))+ #guides(col=FALSE) +
  annotation_custom(p2_inset,xmin=0,xmax=40,ymin=1700,ymax=2600)+ 
  annotate('text',x=13,y=-100,label=expression(R==1 - frac(2* "|" * F[max] - F^'*' * "|",F[max] + "|" * F[max] - F^'*' * "|")),size=4)  +
  annotate('text', x = 45,y = t20_a, label = expression(F[max]),size=4) +
  annotate('text', x = 45,y=t20_b,label=expression(F^'*'),size=4) +
  annotate('segment',x=40.5,xend=43.5,y=t20_b,yend=t20_b,linetype = 2,col = '#7570B3') +
  annotate('segment',x=20.5,xend=42,y=t20_a,yend=t20_a,linetype=2,col = '#7570B3') + guides(col=FALSE) +
  theme(axis.title.y=element_text(vjust=-5))


p3 <- ggboxplot(t_final,x='Community',y='CommunityPhenotype',col='Community',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white',dot.size=0.1) + 
  stat_compare_means(paired=TRUE,comparisons = comparisons,method='t.test', label.y =  c(1400,1600,1800),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))) +
  guides(col=FALSE) +   labs(x = '',y = 'F*') +  
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3')) + guides(fill=FALSE)+
  scale_y_continuous(breaks=c(-500,1500),limits=c(-600,1850)) + 
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=-45,colour=c('#1B9E77','#D95F02','#7570B3')),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0))) 

p4 <- ggboxplot(t_final,x='Community',y='RS',col='Community',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white',height=0) + 
  guides(col=FALSE) +   
  stat_compare_means(paired=TRUE,comparisons = comparisons,method='t.test',label.y=c(1.1,1.25,1.4),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))) +
  labs(x='',y='R') +
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3')) + guides(fill=FALSE)+
  scale_y_continuous(breaks=c(0,1),limits=c(-0.17,1.45)) + 
  theme(axis.title=element_text(size=12),axis.text.x=element_text(angle=-45,colour=c('#1B9E77','#D95F02','#7570B3')),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) 

p5 <- ggboxplot(t_final2,x='Community',y='Fmax2',col='Community',palette = "dark2",
                add = "jitter",jitter.hight=0,legend='right',shape=1,outlier.size=1,outlier.colour='white') + 
  stat_compare_means(paired=TRUE,comparisons = comparisons,method='t.test',label.y=c(2400,2700,3000),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))) +
  # stat_summary(fun.y=mean,mapping = aes(col=Community), geom="point", shape=18, size=5) +
  guides(col=FALSE) +   labs(x = '',y = expression(F[max])) +  
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3')) + guides(fill=FALSE)+
  scale_y_continuous(breaks=c(0,3000),limits=c(0,3100)) + 
  theme(axis.title=element_text(size=12),axis.text.x=element_text(angle=-45,colour=c('#1B9E77','#D95F02','#7570B3')),
        axis.title.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)))

p6 <- ggplot() + 
  geom_point(t_final2[seed==k],mapping = aes(x=R,y=Fmean,col=Community),size= 1,shape=1) +
  geom_point(t_final2[seed!=k],mapping = aes(x=R,y=Fmean,col=Community),size= 1,shape=1) + theme_pubr() +
  labs(x= expression(Mean(R)),y = expression(Mean(F^'*'))) + guides(col=FALSE) +
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3'))  +
  scale_x_continuous(breaks=c(0,1),limits=c(-0.1,1)) +
  scale_y_continuous(breaks = c(0,1500),limits=c(0,1700)) +
  theme(axis.title = element_text(size=12,vjust=15,margin = margin(-15,0,0,0)),
        axis.title.y = element_text(margin = margin(t = 0, r = -20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = -70, r = 0, b = 0, l = 0)))
  

top = ggarrange(p1,p2,labels=c('A','B'),widths=c(5,4))
bottom = ggarrange(p3,p4,p5,p6,labels=c('C','D','E','F'),nrow=1,ncol=4,vjust=0,align = 'h')
ggsave('../Plots/Fig4.png',ggarrange(top,NULL,bottom,nrow=3,ncol=1,heights=c(4,0.1,3)),height=7/1,width=10.5/1.1)