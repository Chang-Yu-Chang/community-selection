library(data.table)
library(ggplot2)
library(ggpubr)
library(operators)
library(gridExtra)
library(RColorBrewer)
library(car)
rm(list=ls())

function_df  = fread('../data/temp/aggregated_short.csv')
function_df = function_df[CommunityPhenotypeName == 'f1_additive' & 
                            Assembly %in% c("Arora2019_V2", "Arora2019_V2_control","Panke_Buisse2015",
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
                                                                                    "simple_screening","ctrl","coalescence",
                                                                                    "pool_top25","select_top25"),]


function_df[Assembly =='Raynaud2019a_V2']$Assembly = 'Raynaud2019a'
function_df[Assembly =='Raynaud2019b_V2']$Assembly = 'Raynaud2019b'
function_df[Assembly =='Raynaud2019a_V2_control']$Assembly = 'Raynaud2019a_control'
function_df[Assembly =='Raynaud2019b_V2_control']$Assembly = 'Raynaud2019b_control'
function_df[Assembly =='Arora2019_V2']$Assembly = 'Arora2019'
function_df[Assembly =='Arora2019_V2_control']$Assembly = 'Arora2019_control'

ctrl = function_df[function_df$Assembly == 'simple_screening' & Transfer ==40]
ctrl_max = ctrl[,max(CommunityPhenotype),by=.(Assembly,CommunityPhenotypeName,SpeciesPool)]
function_df = function_df[SpeciesPool %in% ctrl_max$SpeciesPool]
equilibrium = function_df[Transfer==40,]
equilibrium$Fitness = 0
equilibrium$Fitness2 = 0

for(i in unique(ctrl$SpeciesPool)){
  equilibrium[SpeciesPool==i,]$Fitness = equilibrium[SpeciesPool==i,]$CommunityPhenotype/ctrl_max[SpeciesPool==i,]$V1
}
Performance =  equilibrium[,max(Fitness), by = .(Assembly,CommunityPhenotypeName,SpeciesPool)]
colnames(Performance)[4]  = 'Performance'
order_assembly =c('simple_screening','Swenson2000a',
                    'Blouin2015',
                    'Panke_Buisse2015',
                    'Jochum2019',
                    'Raynaud2019b',
                    'Mueller2019',
                    'Wright2019',
                    'Swenson2000b','Arora2019','Raynaud2019a')
Performance = Performance[Assembly %in% order_assembly]
Performance$Assembly = factor(Performance$Assembly,
                                 levels=order_assembly)
Performance$Method = 'Pooling'
Performance[Assembly %in% c('Swenson2000b','Arora2019','Raynaud2019a')]$Method = 'Single Parent'
pvals =c()
for(j in order_assembly){
  a = Performance[Assembly==j,]$Performance
  pvals = c(pvals,t.test(a-1,alternative ='less')$p.value)
}
p1 <- ggplot(Performance[Assembly != 'ctrl' & Assembly != 'simple_screening'],
             aes(x=Assembly,y=Performance,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0)  + 
  scale_y_continuous(breaks=c(-1,0,1,2),limits=c(-1.2,2)) +   
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90)) + 
  scale_shape_manual(values=c(3,1,2)) + 
  scale_colour_manual(values = c('Orange','Purple')) + geom_text(y=1.2,label = "***",size=3) +
  labs(x = '',y=expression(F[max*(Protocol)]/F[max*(Screen)]))

Spol =2
C = function_df[Assembly =='simple_screening' & SpeciesPool ==Spol & as.numeric(Transfer)>0]
Best_Wells = tail(C[Transfer==1,][order(C[Transfer ==1,]$CommunityPhenotype)],1)$Well
C$Highlight = FALSE
C[Well %in% Best_Wells]$Highlight = TRUE
p2 <- ggplot(C,aes(x=Transfer,y=CommunityPhenotype,group=Well,col=Highlight)) + geom_line() +  
  theme_pubr() + scale_colour_manual(values=c('Grey','Black')) + guides(col=FALSE) + labs(y='Function')+ 
  geom_hline(yintercept= ctrl_max[SpeciesPool ==Spol]$V1,linetype =2,col='Red') 

C2 = function_df[Assembly =='simple_screening' & as.numeric(Transfer)>0]
C2[,rank :=frank(-CommunityPhenotype),by=list(SpeciesPool,Transfer)]
C2[Transfer ==1]
C2_Plot = C2[Transfer ==1]
C2_Plot$offspring_rank = C2[Transfer ==40]$rank
p2b <- ggplot(C2_Plot[SpeciesPool<4],aes(x=rank,y=offspring_rank,
                          col=as.factor(SpeciesPool),
                          shape=as.factor(SpeciesPool))) + 
  # geom_point(C2_Plot[SpeciesPool>4],mapping = aes(x=rank,y=offspring_rank),col='grey',shape=16) +
  geom_point(size=2,stroke=1) + 
  scale_shape_manual(values = c(3,1,2)) + 
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  # scale_colour_manual(values = c('Red','Blue','Green')) +
  labs(x='Rank Function at Transfer 1',y ='Rank Function at Equilibrium')  + 
  theme_pubr() +  scale_colour_brewer(palette='Dark2') +labs(shape = 'Species Pool',col = 'Species Pool')

D = function_df[Assembly =='pool_top25' & SpeciesPool ==Spol & as.numeric(Transfer)>0]
Top_list = D[Transfer==20]
Top_25  = Top_list[rev(order(Top_list$CommunityPhenotype))[1:24]]$Well
Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
D[Transfer>20 & Well %!in% Top_25]$CommunityPhenotype = NA
D$Highlight = FALSE
D[Well %in% Top_1]$Highlight = TRUE
D[Transfer>20]$Highlight = TRUE
D$Well = factor(D$Well,levels=c(Top_1,unique(D$Well)[unique(D$Well) != Top_1]))
p3 <- ggplot(D,aes(x=Transfer,y=CommunityPhenotype,group=rev(Well),col=Highlight)) + geom_line() +  
  theme_pubr() + scale_colour_manual(values=c('Grey','Black')) + guides(col=FALSE) + labs(y='Function') + 
  geom_hline(yintercept= ctrl_max[SpeciesPool ==Spol]$V1,linetype =2,col='Red') 
D2 = function_df[Assembly =='pool_top25']
D2[,maximum :=max(CommunityPhenotype),by=list(SpeciesPool,Transfer)]
D2[,variance :=var(CommunityPhenotype),by=list(SpeciesPool,Transfer)]

D2_Plot = D2[Transfer ==20]
D2_Plot$offspring_maximum = D2[Transfer==40]$maximum
D2_Plot$offspring_Variance = D2[Transfer==40]$variance

p3b <- ggplot(D2_Plot[SpeciesPool<4], 
              aes(x=maximum,y=offspring_maximum,
              col=as.factor(SpeciesPool),
              shape=as.factor(SpeciesPool)))  +
  geom_point(D2_Plot[SpeciesPool>=4],mapping = aes(x= maximum,y=offspring_maximum),col='grey',shape=16,size=2) +
  geom_point(size=4,stroke=1) + 
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Pooling', y ='Max Function After Pooling') + 
  lims(x=c(-350,1250),y=c(-350,1250))

D2_Plot$offspring_Variance = D2[Transfer==40]$variance

p3c <- ggplot(D2_Plot[SpeciesPool<4], 
              aes(x=variance,y=offspring_Variance,
                  col=as.factor(SpeciesPool),
                  shape=as.factor(SpeciesPool)))  +
  geom_point(D2_Plot[SpeciesPool>=4],mapping = aes(x= variance,y=offspring_Variance),col='grey',shape=16,size=2) +
  geom_point(size=4,stroke=1) + 
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Variance Before', y ='Variance After') + 
  scale_x_continuous(limits=c(0,150000)) + scale_y_continuous(limits=c(0,150000)) +
  guides(col=FALSE,shape=FALSE) + theme(axis.text=element_blank(),axis.title = element_text(size=8))
p3b = p3b + annotation_custom(ggplotGrob(p3c),xmin =-350,xmax =200,ymin=500,ymax=1250)
p =0.25
E = function_df[Assembly =='select_top25' & SpeciesPool ==Spol & as.numeric(Transfer)>0]
Top_list = E[Transfer==20]
# Top_25  = Top_list[rev(order(Top_list$CommunityPhenotype))[1:24]]$Well
# Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
E$Highlight = FALSE
# E[Well %in% Top_1]$Highlight = TRUE
# E$Well = factor(E$Well,levels=c(Top_1,unique(E$Well)[unique(E$Well) != Top_1]))
sorted_community_function = Top_list[order(Top_list$CommunityPhenotype)]$CommunityPhenotype
cut_off = sorted_community_function[round(length(sorted_community_function) *(1-p))]
winners = Top_list[Top_list$CommunityPhenotype >= cut_off]
old_wells = rep(rev(winners$Well),1/p)
new_wells = unique(E$Well)
vec=c()
E$Old_Well = E$Well
E$New_Well = E$Well
for(j in new_wells){
  E[Transfer>20 & Well ==j]$Old_Well = old_wells[which(new_wells == j)]
  
}

# Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
Top_1  = Top_list[rev(order(Top_list$CommunityPhenotype))[1]]$Well
E[Old_Well %in% Top_1]$Highlight = TRUE

p4 <- ggplot(E,aes(x=Transfer,y=CommunityPhenotype,group=Old_Well,col=Highlight)) + 
  geom_line() +  
  theme_pubr() + scale_colour_manual(values=c('Grey','Black')) + guides(col=FALSE) + labs(y='Function') + 
  geom_hline(yintercept= ctrl_max[SpeciesPool ==Spol]$V1,linetype =2,col='Red') 


E2 = function_df[Assembly =='select_top25']
E2[,maximum :=max(CommunityPhenotype),by=list(SpeciesPool,Transfer)]
E2_Plot = E2[Transfer ==20]
E2_Plot$offspring_maximum = E2[Transfer==40]$maximum
p4b <- ggplot(E2_Plot[SpeciesPool<4], 
              aes(x=maximum,y=offspring_maximum,
                  col=as.factor(SpeciesPool),
                  shape=as.factor(SpeciesPool)))  +
  geom_point(E2_Plot[SpeciesPool>=4],mapping = aes(x= maximum,y=offspring_maximum),col='grey',shape=16,size=2) +
  geom_point(size=4,stroke=1) + 
  scale_shape_manual(values = c(3,1,2)) +
  scale_colour_brewer(palette='Dark2') +
  labs(shape = 'Species Pool',col = 'Species Pool') +
  theme_pubr() +
  geom_abline(intercept = 0,slope = 1,col='Red',linetype=2) +
  labs(x = 'Max Function Before Selection', y ='Max Function After Selection') + 
  lims(x=c(400,1250),y=c(400,1250))



Example_DF = function_df[(Assembly == 'Blouin2015' | Assembly == 'Blouin2015_control' |Assembly == 'simple_screening') 
                         & SpeciesPool ==1,]
Example_DF[,Mean:=mean(CommunityPhenotype),by=list(Assembly,Transfer)]
Example_DF[,UCI:=mean(CommunityPhenotype) + sd(CommunityPhenotype),by=list(Assembly,Transfer)]
Example_DF[,LCI:=mean(CommunityPhenotype) - sd(CommunityPhenotype),by=list(Assembly,Transfer)]
Example_DF[,Maximum:=max(CommunityPhenotype),by=list(Assembly,Transfer)]
Example_DF = Example_DF[Well == 'W0']

p5 <- ggplot(Example_DF[Transfer>0,])  +
  annotate("rect",xmin=-Inf,xmax=21,ymin=-Inf,ymax=Inf,alpha=0.2,fill='Grey') +
  annotate("text",x=10.5,y=750,label='Selection',size=8) +
  annotate("text",x=30.5,y=750,label='Stabilization',size=8) + 
  geom_hline(yintercept= ctrl_max[SpeciesPool ==1]$V1,linetype =2,col='Red')  +
  
  # geom_rect(xmin=21,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.02,fill='brown') +
  geom_line(aes(x=Transfer,y=Mean,col=Assembly),size=1)+ 
  geom_point(aes(x=Transfer,y=Mean,col=Assembly),size=2) +
  labs(y = 'Mean Function',x = 'Generations(Transfers)',col='') + 
  theme_pubr() +
  scale_colour_brewer(palette='Dark2',labels = c('Blouin 2015 Protocol','Blouin 2015 Control','Screen'))

p6 <- ggplot(Example_DF[Transfer>0,])  +
  annotate("rect",xmin=-Inf,xmax=21,ymin=-Inf,ymax=Inf,alpha=0.2,fill='Grey') +
  annotate("text",x=10.5,y=750,label='Selection',size=8) +
  annotate("text",x=30.5,y=750,label='Stabilization',size=8) +
  geom_hline(yintercept= ctrl_max[SpeciesPool ==1]$V1,linetype =2,col='Red') +
  # geom_rect(xmin=21,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.02,fill='brown') +
  geom_line(aes(x=Transfer,y=Maximum,col=Assembly),size=1)+ 
  geom_point(aes(x=Transfer,y=Maximum,col=Assembly),size=2) +
  labs(y = 'Max Function',x = 'Generations(Transfers)',col='') + 
  theme_pubr() +
  scale_colour_brewer(palette='Dark2',labels = c('Blouin 2015 Protocol','Blouin 2015 Control','Screen'))

Normal = function_df[Assembly %in% c("Arora2019","Blouin2015",
                                     "Raynaud2019a","Raynaud2019b",
                                                  "Swenson2000a","Swenson2000b","Wright2019")]
Normal[,Maximum:=max(CommunityPhenotype),by=list(Assembly,Transfer,SpeciesPool)]
Normal = Normal[Well == 'W0' & Transfer == 40]
Normal$Experiment = Normal$Assembly
Normal$Type = 'Experiment'
Control = function_df[Assembly %in% c("Arora2019_control","Blouin2015_control",
                                      "Raynaud2019a_control","Raynaud2019b_control",
                                      "Swenson2000a_control","Swenson2000b_control","Wright2019_control")]
Control[,Maximum:=max(CommunityPhenotype),by=list(Assembly,Transfer,SpeciesPool)]
Control = Control[Well == 'W0' & Transfer == 40]
Control$Experiment = Normal$Assembly
Control$Type = 'Control'
Screen = function_df[Assembly %in% c("simple_screening")]
Screen[,Maximum:=max(CommunityPhenotype),by=list(Assembly,Transfer,SpeciesPool)]
Screen = Screen[Well == 'W0' & Transfer == 40]

Screen2 = Control
Screen2$Maximum = rep(Screen$Maximum,7)
Screen2$Type = 'Screen'

Final = rbind(Normal,Control,Screen2)
ann_text = data.frame(Type  = 1.5,
                      Maximum=1500,
                      Lab = rep('hey',7),
                      Experiment = unique(Final$Experiment),
                      SpeciesPool = 1,stringsAsFactors = FALSE)

for(j in 1:nrow(ann_text)){
  test = t.test(Normal[Experiment == ann_text$Experiment[j]]$Maximum,Control[Experiment == ann_text$Experiment[j]]$Maximum,paired=TRUE)
  test2 = t.test(Normal[Experiment == ann_text$Experiment[j]]$Maximum,Screen2[Experiment == ann_text$Experiment[j]]$Maximum,paired=TRUE)
  
  ann_text$Lab[j]  = paste('p = ',signif(test$p.value,3))
  ann_text$Lab2[j]  = paste('p = ',signif(test2$p.value,3))
  
}


t = ggpaired(Final[Type %in% c('Control','Experiment')], x = "Type", y = "Maximum",
         line.color = "gray", line.size = 0.1,
         palette = "jco")+
  stat_compare_means(paired = TRUE) + guides(col=FALSE) + scale_y_continuous(limits=c(-1100,1500)) +
  labs(x='',y='Max Function') +
  theme(axis.text.x = element_text(angle=315,vjust=0.4,size=8))  + facet_grid(~Experiment)
Final[,Mean:=max(Maximum),by=list(Assembly,Transfer)]
t = Final[SpeciesPool==1 & Type %in% c('Control','Experiment')]
p7 <- ggplot(Final[Type %in% c('Control','Experiment')],aes(x = Type,y=Maximum,group=SpeciesPool)) + 
  geom_point(size=1) + 
  geom_line(size=0.1) +
  theme_pubr() + 
  labs(x='',y = 'Max Function')  +
  geom_text(ann_text,mapping = aes(label=Lab),size=2)+ 
    facet_grid(~Experiment) +
 theme(axis.text.x = element_text(angle=315,vjust=0.4,size=8),strip.text.x = element_text(size = 6))



top = ggarrange(p5,p6,common.legend = TRUE,labels="AUTO")
left = ggarrange(p1,p7,labels=c('C','D'),ncol=1,heights=c(2,1))
middle = ggarrange(p2,p3,p4,labels=c('E','G','I'),ncol=1,nrow=3)
right = ggarrange(p2b,p3b,p4b,labels=c('F','H','J'),ncol=1,nrow=3,common.legend = TRUE,legend = 'none')
ggsave('../figure/Fig1.png',grid.arrange(top,left,middle,right,layout_matrix = rbind(c(1,1,1),c(2,3,4),c(2,3,4))),
       width=20,height=15)

