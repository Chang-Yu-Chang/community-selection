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

A =merge(rbindlist(lapply(mapping_file[protocol == 'pool_top25' & monoculture == FALSE & is.na(bottleneck_size)]$file,fread)),mapping_file)
A[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
A[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
A[,variance :=var(CommunityPhenotype),by=list(seed,Transfer)]
A[,mean :=mean(CommunityPhenotype),by=list(seed,Transfer)]

B =merge(rbindlist(lapply(mapping_file[protocol == 'select_top25' & monoculture == FALSE & is.na(bottleneck_size)]$file,fread)),mapping_file)
B[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
B[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
B[,variance :=var(CommunityPhenotype),by=list(seed,Transfer)]
B[,mean :=mean(CommunityPhenotype),by=list(seed,Transfer)]

top1 = A[Transfer==40 & seed ==k & rank ==1]$Well
top2 = A[Transfer==20 & seed ==k & rank ==1]$Well
top_25 = A[Transfer==20 & seed ==k & rank <=24]$Well
A$Highlight = FALSE
A[Well == top1 & Transfer>20 & seed ==k]$Highlight = TRUE
A[Well == top2 & Transfer <=20 & seed ==k]$Highlight = TRUE
A[Highlight == TRUE]$Well  = 'W100'
A[Transfer>20 & Highlight ==FALSE]$Well = paste(A[Transfer>20 & Highlight ==FALSE]$Well,'_2',sep='')
t_1 = A[Transfer == 20 & Well%in% top_25]
# A[Transfer>20 & Well %!in% top_25]$CommunityPhenotype = NA
# A$Well = factor(A$Well,levels=c(top,unique(A$Well)[unique(A$Well) != top]))
p1 <- ggplot() + 
  geom_line(A[Transfer>0 & seed==k & Highlight == FALSE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray90') + 
  geom_line(A[Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20') + 
  geom_point(A[Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20',size=1,shape=1) + 
  theme_pubr() + labs(y='F',x='Generation')+ 
  # geom_hline(yintercept= ctrl_max,linetype =2,col='Red')  +
  scale_y_continuous(breaks=c(-1000,0,1000),limits=c(-1100,1100))+
  scale_x_continuous(breaks=c(0,20,40)) +  
  annotate('text',x=20,y=1000,label=expression('Pool top 24 communitites ('*D == 10^3*')')) +
  annotate("segment", x = 20, xend = 20, y = 900, yend = 800, colour = "Black", size=1, arrow=arrow(length = unit(0.2, "cm")))

A$parent_maximum = 0
A$parent_variance = 0
A$parent_mean = 0


A[Transfer==40]$parent_maximum = A[Transfer==20]$maximum
A[Transfer==40]$parent_variance = A[Transfer==20]$variance
A[Transfer==40]$parent_mean = A[Transfer==20]$mean

p2 <- ggplot()  +
  geom_point(A[seed!=k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='lightskyblue1',shape=1,size= 1) +
  geom_point(A[seed==k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='blue3',size= 1,shape=1,stroke=1) +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max]~ Before ~ Pooling), y = expression(F[max]~  After~ Pooling)) + 
  scale_x_continuous(limits = c(0,2500),breaks=c(0,2500)) + scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))

p2a <- ggplot()  +
  geom_point(A[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_variance,y=variance),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(A[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_variance,y=variance),col='blue3',shape=16,size= 0.3) +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() + guides(col=FALSE,shape=FALSE) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(Var(F)~ Before), y = expression(Var(F)~ After)) + 
  scale_x_continuous(limits=c(0,250000)) + scale_y_continuous(limits=c(0,250000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))


p2b <- ggplot()  +
  geom_point(A[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_mean,y=mean),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(A[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_mean,y=mean),col='blue3',shape=16,size= 0.3) +  geom_point(col='lightskyblue1',shape=16,size= 0.3) +
  theme_pubr() + 
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression( Mean(F)~ Before), y = expression( Mean(F)~ After)) + 
  scale_x_continuous(limits=c(-550,1000)) + scale_y_continuous(limits=c(-550,1000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))

p = 0.25
Top_list = B[Transfer==20 & seed==k]
B$Highlight = FALSE
old_wells =  B[Transfer==20 & seed==k & rank<=24]$Well
old_wells = rep(rev(old_wells),1/p)
new_wells = unique(B$Well)
vec=c()
B$Old_Well = B$Well
B$New_Well = B$Well
for(j in new_wells){
  B[Transfer>20 & Well ==j]$Old_Well = old_wells[which(new_wells == j)]
  
}

# Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
B[Old_Well %in% Top_1]$Highlight = TRUE



p3 <- ggplot() + 
  geom_line(B[Transfer>0 & seed==k & Highlight == FALSE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Old_Well),col='Gray90') + 
  geom_line(B[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Old_Well),col='Gray20') + 
  geom_point(B[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Old_Well),col='Gray20',size=1,shape=1) +
  theme_pubr() + labs(y='F',x='Generation')+
  # geom_hline(yintercept= ctrl_max,linetype =2,col='Red')  +
  scale_y_continuous(breaks=c(-1000,0,1000),limits=c(-1100,1100))+
  scale_x_continuous(breaks=c(0,20,40)) +  
  annotate('text',x=20,y=1000,label=expression('Select top 24 communitites ('*d == 10^3*')')) +
  annotate("segment", x = 20, xend = 20, y = 900, yend = 800, colour = "Black", size=1, arrow=arrow(length = unit(0.2, "cm")))

B$parent_maximum = 0
B$parent_variance = 0
B$parent_mean = 0

B[Transfer==40]$parent_maximum = B[Transfer==20]$maximum
B[Transfer==40]$parent_variance = B[Transfer==20]$variance
B[Transfer==40]$parent_mean = B[Transfer==20]$mean

p4 <- ggplot()  +
  geom_point(B[seed!=k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='lightskyblue1',shape=1,size= 1) +
  geom_point(B[seed==k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='blue3',size= 1,shape=1,stroke=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max]~ Before~ Selection), y = expression(F[max]~ After~ Selection)) + 
  scale_x_continuous(limits = c(0,2500),breaks=c(0,2500)) + scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))


p4a <- ggplot()  +
  geom_point(B[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_variance,y=variance),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(B[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_variance,y=variance),col='blue3',shape=16,size= 0.3) +
  theme_pubr() + guides(col=FALSE,shape=FALSE) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(Var(F)~ Before), y = expression(Var(F)~ After)) + 
  scale_x_continuous(limits=c(0,250000)) + scale_y_continuous(limits=c(0,250000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))


p4b <- ggplot()  +
  geom_point(B[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_mean,y=mean),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(B[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_mean,y=mean),col='blue3',shape=16,size= 0.3) +  geom_point(col='lightskyblue1',shape=16,size= 0.3) +
  theme_pubr() + guides(col=FALSE,shape=FALSE) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression( Mean(F)~ Before), y = expression( Mean(F)~ After)) + 
  scale_x_continuous(limits=c(-500,1000)) + scale_y_continuous(limits=c(-500,1000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))


A =merge(rbindlist(lapply(mapping_file[protocol == 'pool_top25' & monoculture == FALSE & bottleneck_size  == 1e-06]$file,fread)),mapping_file)
A[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
A[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
A[,variance :=var(CommunityPhenotype),by=list(seed,Transfer)]
A[,mean :=mean(CommunityPhenotype),by=list(seed,Transfer)]

B =merge(rbindlist(lapply(mapping_file[protocol == 'select_top25' & monoculture == FALSE & bottleneck_size  == 1e-05]$file,fread)),mapping_file)
B[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer)]
B[,maximum :=max(CommunityPhenotype),by=list(seed,Transfer)]
B[,variance :=var(CommunityPhenotype),by=list(seed,Transfer)]
B[,mean :=mean(CommunityPhenotype),by=list(seed,Transfer)]


top1 = A[Transfer==40 & seed ==k & rank ==1]$Well
top2 = A[Transfer==20 & seed ==k & rank ==1]$Well
# top_25 = A[Transfer==20 & seed ==k & rank <=24]$Well
A$Highlight = FALSE
A[Well == top1 & Transfer>20 & seed ==k]$Highlight = TRUE
A[Well == top2 & Transfer <=20 & seed ==k]$Highlight = TRUE
A[Highlight == TRUE]$Well  = 'W100'
A[Transfer>20 & Highlight ==FALSE]$Well = paste(A[Transfer>20 & Highlight ==FALSE]$Well,'_2',sep='')

# A[Transfer>20 & Well %!in% top_25]$CommunityPhenotype = NA
# A$Well = factor(A$Well,levels=c(top,unique(A$Well)[unique(A$Well) != top]))
p5 <- ggplot() + 
  geom_line(A[Transfer>0 & seed==k & Highlight == FALSE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray90') + 
  geom_line(A[Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20') + 
  geom_point(A[Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Well),col='Gray20',size=1,shape=1) + 
  theme_pubr() + labs(y='F',x='Generation')+ 
  # geom_hline(yintercept= ctrl_max,linetype =2,col='Red')  +
  scale_y_continuous(breaks=c(-1000,0,1000),limits=c(-1100,1100))+
  scale_x_continuous(breaks=c(0,20,40)) +  
  annotate('text',x=20,y=1000,label=expression('Pool top 24 communitites ('*d == 10^9*')')) +
  annotate("segment", x = 20, xend = 20, y = 900, yend = 800, colour = "Black", size=1, arrow=arrow(length = unit(0.2, "cm")))




A$parent_maximum = 0
A$parent_variance = 0
A$parent_mean = 0


A[Transfer==40]$parent_maximum = A[Transfer==20]$maximum
A[Transfer==40]$parent_variance = A[Transfer==20]$variance
A[Transfer==40]$parent_mean = A[Transfer==20]$mean

p6 <- ggplot()  +
  geom_point(A[seed!=k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='lightskyblue1',shape=1,size= 1) +
  geom_point(A[seed==k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='blue3',size= 1,shape=1,stroke=1) +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max]~ Before ~ Pooling), y = expression(F[max]~  After~ Pooling)) + 
  scale_x_continuous(limits = c(0,2500),breaks=c(0,2500)) + scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))

p6a <- ggplot()  +
  geom_point(A[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_variance,y=variance),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(A[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_variance,y=variance),col='blue3',shape=16,size= 0.3) +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() + guides(col=FALSE,shape=FALSE) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(Var(F)~ Before), y = expression(Var(F)~ After)) + 
  scale_x_continuous(limits=c(0,250000)) + scale_y_continuous(limits=c(0,250000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))


p6b <- ggplot()  +
  geom_point(A[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_mean,y=mean),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(A[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_mean,y=mean),col='blue3',shape=16,size= 0.3) +  geom_point(col='lightskyblue1',shape=16,size= 0.3) +
  theme_pubr() + 
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression( Mean(F)~ Before), y = expression( Mean(F)~ After)) + 
  scale_x_continuous(limits=c(-550,1000)) + scale_y_continuous(limits=c(-550,1000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))

p = 0.25
Top_list = B[Transfer==20 & seed==k]
Top_list2 = B[Transfer==40 & seed==k]

B$Highlight = FALSE
old_wells =  B[Transfer==20 & seed==k & rank<=24]$Well
old_wells = rep(rev(old_wells),1/p)
new_wells = unique(B$Well)
vec=c()
B$Old_Well = B$Well
B$New_Well = B$Well
# for(j in new_wells){
#   B[Transfer>20 & Well ==j]$Old_Well = old_wells[which(new_wells == j)]
#   
# }

# Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
Top_2  = Top_list2[rev(order(Top_list2$CommunityPhenotype))[1]]$Well

B[Old_Well %in% Top_1 & Transfer<=20]$Highlight = TRUE
B[Old_Well %in% Top_2 & Transfer>20]$Highlight = TRUE
B[Highlight==TRUE]$Old_Well = 'W100'




p7 <- ggplot() + 
  geom_line(B[Transfer>0 & seed==k & Highlight == FALSE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Old_Well),col='Gray90') + 
  geom_line(B[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Old_Well),col='Gray20') + 
  geom_point(B[Transfer>0 & seed==k & Highlight == TRUE],mapping = aes(x=Transfer,y=CommunityPhenotype,group=Old_Well),col='Gray20',size=1,shape=1) +
  theme_pubr() + labs(y='F',x='Generation')+
  # geom_hline(yintercept= ctrl_max,linetype =2,col='Red')  +
  scale_y_continuous(breaks=c(-1000,0,1000),limits=c(-1100,1100))+
  scale_x_continuous(breaks=c(0,20,40)) +  
  annotate('text',x=20,y=1100,label=expression('Select top 24 communitites ('*d == 10^8*')')) +
  annotate("segment", x = 20, xend = 20, y = 900, yend = 800, colour = "Black", size=1, arrow=arrow(length = unit(0.2, "cm")))

B$parent_maximum = 0
B$parent_variance = 0
B$parent_mean = 0

B[Transfer==40]$parent_maximum = B[Transfer==20]$maximum
B[Transfer==40]$parent_variance = B[Transfer==20]$variance
B[Transfer==40]$parent_mean = B[Transfer==20]$mean

p8 <- ggplot()  +
  geom_point(B[seed!=k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='lightskyblue1',shape=1,size= 1) +
  geom_point(B[seed==k  & Transfer==40],mapping = aes(x= parent_maximum,y=maximum),col='blue3',size= 1,shape=1,stroke=1) +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(F[max]~ Before~ Selection), y = expression(F[max]~ After~ Selection)) + 
  scale_x_continuous(limits = c(0,2500),breaks=c(0,2500)) + scale_y_continuous(limits=c(0,2500),breaks=c(0,2500))


p8a <- ggplot()  +
  geom_point(B[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_variance,y=variance),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(B[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_variance,y=variance),col='blue3',shape=16,size= 0.3) +
  theme_pubr() + guides(col=FALSE,shape=FALSE) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression(Var(F)~ Before), y = expression(Var(F)~ After)) + 
  scale_x_continuous(limits=c(0,250000)) + scale_y_continuous(limits=c(0,250000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))


p8b <- ggplot()  +
  geom_point(B[Transfer==40 & seed !=k], 
             mapping = aes(x=parent_mean,y=mean),col='lightskyblue1',shape=16,size= 0.3) +
  geom_point(B[Transfer==40 & seed ==k], 
             mapping = aes(x=parent_mean,y=mean),col='blue3',shape=16,size= 0.3) +  geom_point(col='lightskyblue1',shape=16,size= 0.3) +
  theme_pubr() + guides(col=FALSE,shape=FALSE) +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = expression( Mean(F)~ Before), y = expression( Mean(F)~ After)) + 
  scale_x_continuous(limits=c(-500,1000)) + scale_y_continuous(limits=c(-500,1000)) + theme(axis.text=element_blank(),axis.title = element_text(size=6))
p2_new = p2 + annotation_custom(ggplotGrob(ggarrange(p2b,p2a)),xmin =-150,xmax =1650,ymin=1550,ymax=2750)
p4_new = p4 + annotation_custom(ggplotGrob(ggarrange(p4b,p4a)),xmin =-150,xmax =1650,ymin=1550,ymax=2750)
p6_new = p6 + annotation_custom(ggplotGrob(ggarrange(p6b,p6a)),xmin =-150,xmax =1650,ymin=1550,ymax=2750)
p8_new = p8 + annotation_custom(ggplotGrob(ggarrange(p8b,p8a)),xmin =-150,xmax =1650,ymin=1550,ymax=2750)
left = ggarrange(p1,p2_new,p3,p4_new,labels=c('A','B','C','D'))
# left = annotate_figure(left,
#                 top = text_grob("High Infant Population Size", color = "Black", face = "bold", size = 14))
# right = ggarrange(p5,p6_new,p7,p8_new,labels=c('E','F','G','H'))
# right = annotate_figure(right,
#                        top = text_grob("Low Infant Population Size", color = "Black", face = "bold", size = 14))
ggsave('../Plots/FigS7.png',width=15/2,height=7)


