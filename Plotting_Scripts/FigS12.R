rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
library(operators)

equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    signif(seq(min(x)+d, max(x)-d, length=n),digits=2)
  }
}


equal_breaks2 <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    abs_max = max(abs(min(x)+d),abs(max(x)-d))
    signif(c(-abs_max,0,abs_max),digits=2)
  }
}

equal_limits <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    abs_max = max(abs(min(x)+d),abs(max(x)-d))
    c(0,signif(abs_max*1.75,digits=2))
  }
}

#### Figure 1 ####
mapping_file = fread('../Data/Mapping_Files_Rebuttal/input_independent.csv')
mapping_file$file = paste('../Data/Raw_Rebuttal/',mapping_file$exp_id,'_function.txt',sep='')
mapping_file[cost_mean==0 & selected_function=='f1_additive',]$selected_function = 'Additive'
# mapping_file[cost_mean==0 & selected_function=='f1a_additive',]$selected_function = 'Additive Sparse'
mapping_file[cost_mean==0 & selected_function=='f2_interaction',]$selected_function = 'Non-Additive'
# mapping_file[cost_mean==0 & selected_function=='f2a_interaction',]$selected_function = 'Non-Additive Sparse'
mapping_file[cost_mean==0 & selected_function=='f6_target_resource',]$selected_function = 'Target Resource'
mapping_file[cost_mean==0 & selected_function=='f6a_target_resource',]$selected_function = 'Target Resource'
mapping_file[cost_distribution=='Uniform',]$selected_function = 'Costly'
mapping_file[cost_mean==0 & selected_function=='f5_invader_suppression',]$selected_function = 'Invader Resistance'
mapping_file = mapping_file[l ==0 & phi_mean ==0 & metacommunity_sampling == 'Power' & response == 'type III',]
mapping_file = mapping_file[phi_distribution=='Norm' | selected_function=='Costly',]
mapping_file = mapping_file[selected_function!='f1a_additive' & selected_function!='f2a_interaction']

mapping_file2 = fread('../Data/Mapping_Files_Rebuttal/input_iteration.csv')
mapping_file2$file = paste('../Data/Raw_Rebuttal/',mapping_file2$exp_id,'_function.txt',sep='')
mapping_file2[cost_mean==0 & selected_function=='f1_additive',]$selected_function = 'Additive'
# mapping_file2[cost_mean==0 & selected_function=='f1a_additive',]$selected_function = 'Additive Sparse'
mapping_file2[cost_mean==0 & selected_function=='f2_interaction',]$selected_function = 'Non-Additive'
# mapping_file2[cost_mean==0 & selected_function=='f2a_interaction',]$selected_function = 'Non-Additive Sparse'
mapping_file2[cost_mean==0 & selected_function=='f6_target_resource',]$selected_function = 'Target Resource'
mapping_file2[cost_mean==0 & selected_function=='f6a_target_resource',]$selected_function = 'Target Resource'
mapping_file2[cost_distribution=='Uniform',]$selected_function = 'Costly'
mapping_file2[cost_mean==0 & selected_function=='f5_invader_suppression',]$selected_function = 'Invader Resistance'
mapping_file2 = mapping_file2[l ==0 & phi_mean ==0 & metacommunity_sampling == 'Power' & response == 'type III',]
mapping_file2 = mapping_file2[phi_distribution=='Norm' | selected_function=='Costly',]
mapping_file2 = mapping_file2[selected_function!='f1a_additive' & selected_function!='f2a_interaction']

mapping_file3 = fread('../Data/Mapping_Files_Rebuttal/input_robustness.csv')
mapping_file3$file = paste('../Data/Raw_Rebuttal/',mapping_file3$exp_id,'_function.txt',sep='')
mapping_file3[cost_mean==0 & selected_function=='f1_additive',]$selected_function = 'Additive'
# mapping_file3[cost_mean==0 & selected_function=='f1a_additive',]$selected_function = 'Additive Sparse'
mapping_file3[cost_mean==0 & selected_function=='f2_interaction',]$selected_function = 'Non-Additive'
# mapping_file3[cost_mean==0 & selected_function=='f2a_interaction',]$selected_function = 'Non-Additive Sparse'
mapping_file3[cost_mean==0 & selected_function=='f6_target_resource',]$selected_function = 'Target Resource'
mapping_file3[cost_mean==0 & selected_function=='f6a_target_resource',]$selected_function = 'Target Resource'
mapping_file3[cost_distribution=='Uniform',]$selected_function = 'Costly'
mapping_file3[cost_mean==0 & selected_function=='f5_invader_suppression',]$selected_function = 'Invader Resistance'
mapping_file3 = mapping_file3[l ==0 & phi_mean ==0 & metacommunity_sampling == 'Power' & response == 'type III',]
mapping_file3 = mapping_file3[phi_distribution=='Norm' | selected_function=='Costly',]
mapping_file3 = mapping_file3[selected_function!='f1a_additive' & selected_function!='f2a_interaction']
mapping_file3 = mapping_file3[!is.na(n_migration)]
mapping_file3 = mapping_file3[!is.na(n_migration)]
df1 = mapping_file[protocol %in% c('Blouin2015','Jochum2019',
                                    'Mueller2019','Panke_Buisse2015',
                                    'Swenson2000a','Swenson2000b',
                                    'Chang2020a',
                                    'Chang2020b','Arora2019',
                                    'Raynaud2019a','Raynaud2019b','Wright2019')]
df1$protocol = factor(df1$protocol,levels=c('Swenson2000a','Blouin2015',
                                              'Panke_Buisse2015','Jochum2019',
                                              'Raynaud2019b','Mueller2019',
                                              'Wright2019','Swenson2000b','Arora2019','Raynaud2019a','Chang2020a','Chang2020b'))
df1$Performance_Max = NA
df1$Performance_Mean = NA
# df1 = df1[df1$selected_function != 'Non-Additive Rugged',]
for(j in 1:nrow(df1)){
  k = df1$seed[j]
  screen = fread(mapping_file[seed==k & 
                                protocol=='simple_screening' &
                                monoculture==FALSE & 
                                selected_function ==df1$selected_function[j]]$file)
  ref_max = max(screen[Transfer==40]$CommunityPhenotype)
  ref_mean = mean(screen[Transfer==40]$CommunityPhenotype)
  temp_df = fread(df1$file[j])[Transfer==40]
  df1$Performance_Max[j] = max(temp_df$CommunityPhenotype) -ref_max
  df1$Performance_Mean[j] = mean(temp_df$CommunityPhenotype) - ref_mean
  df1$SR[j] = temp_df[temp_df$CommunityPhenotype == max(temp_df$CommunityPhenotype)]$Richness
  df1$SR_Screen[j]  = screen[Transfer==40 & CommunityPhenotype == ref_max]$Richness
  df1$Biomass[j] = temp_df[temp_df$CommunityPhenotype == max(temp_df$CommunityPhenotype)]$Biomass
  df1$Biomass_Screen[j]  = screen[Transfer==40 & CommunityPhenotype == ref_max]$Biomass
}

df1$Method = 'Migrant\nPool'
df1[protocol%in% c('Chang2020a','Chang2020b','Swenson2000b','Arora2019','Raynaud2019a')]$Method = 'Propagule'

#### Figure 2 ####
df2 = mapping_file[protocol=='directed_selection',]
df2$DE_Strategy = 'Other'
df2[directed_selection==TRUE & bottleneck == TRUE & bottleneck_size == 1e-4,]$DE_Strategy = 'Bottleneck'
df2[directed_selection==TRUE & knock_in == TRUE & knock_in_threshold == 0.95,]$DE_Strategy  = 'Species Knock-in'
df2[directed_selection==TRUE & knock_out == TRUE,]$DE_Strategy  = 'Species Knock-Out'
df2[directed_selection==TRUE & migration ==TRUE & is.na(s_migration),]$DE_Strategy  = 'Migration from Pool'
df2[directed_selection==TRUE & coalescence == TRUE,]$DE_Strategy  = 'Coalescence'
df2[directed_selection==TRUE & resource_shift == TRUE & r_percent == 0.5,]$DE_Strategy = 'Resource Shift'
df2 = df2[df2$DE_Strategy != 'Other',]
df2$Parent_Max = NA
df2$Offspring_Max = NA
df2$Screen_Max = NA
for(j in 1:nrow(df2)){
  k = df2$seed[j]
  screen = fread(mapping_file[seed==k & 
                                protocol=='simple_screening' &
                                monoculture==FALSE & 
                                selected_function ==df2$selected_function[j]]$file)
  temp_df = fread(df2$file[j])
  df2$Screen_Max[j] = max(screen[Transfer==40]$CommunityPhenotype)
  df2$Parent_Max[j] = max(temp_df[Transfer==20]$CommunityPhenotype)
  df2$Offspring_Max[j] = max(temp_df[Transfer==40]$CommunityPhenotype)
  df2$Screen_Mean[j] = mean(screen[Transfer==40]$CommunityPhenotype)
  df2$Parent_Mean[j] = mean(temp_df[Transfer==20]$CommunityPhenotype)
  df2$Offspring_Mean[j] = mean(temp_df[Transfer==40]$CommunityPhenotype)
  
}

# df2 = df2[!(DE_Strategy == 'Resource Shift' & selected_function == 'Target Resource')]  #Rshift strategy not applicable to this function
df2$protocol  = df2$DE_Strategy
df2$Performance_Mean = df2$Offspring_Mean-df2$Screen_Mean
df2$Performance_Max = df2$Offspring_Max-df2$Screen_Max
df2$PO = df2$Offspring_Max-df2$Parent_Max

df3 = mapping_file[directed_selection==TRUE & bottleneck == TRUE  & protocol == 'directed_selection']
df3 =merge(rbindlist(lapply(df3$file,fread)),df3)
df3[,rank :=frank(-CommunityPhenotype),by=list(seed,Transfer,bottleneck_size,selected_function)]
df3 = df3[bottleneck_size %!in% c(1e-4,1e-5)]
df3 = df3[Transfer==40]
df3$Popsize_mean = 1000*df3$scale*df3$bottleneck_size*df3$dilution
df3[,F_Mean := mean(CommunityPhenotype),by = list(seed,Transfer,bottleneck_size,selected_function)]
df3[,F_Max := max(CommunityPhenotype),by = list(seed,Transfer,bottleneck_size,selected_function)]
df3[,Mean_F_Mean := mean(F_Mean),by = list(Popsize_mean,selected_function)]
df3[,SD_F_Mean := sd(F_Mean),by = list(Popsize_mean,selected_function)]
df3[,Mean_F_Max := mean(F_Max),by = list(Popsize_mean,selected_function)]
df3[,SD_F_Max := sd(F_Max),by = list(Popsize_mean,selected_function)]
df3 = df3[Well == 'W0']



#### Figure 3 ####
df4 = mapping_file2[protocol=='simple_screening']
df4_control = df4[grep('simple_screening',df4$exp_id)]
df4$Treatment = 'Other'
df4[grep('iteration_1',df4$exp_id)]$Treatment = 'Bottleneck'
df4[grep('iteration_6',df4$exp_id)]$Treatment = 'Migration'
df4[grep('iteration_5',df4$exp_id)]$Treatment = 'Both'
df4 = df4[Treatment !='Other']

df4_control = merge(rbindlist(lapply(df4_control$file,fread)),df4_control)
df4_control[,Control_Maximum:=max(CommunityPhenotype),by=list(exp_id,Transfer,selected_function)]
df4_control = df4_control[Well == 'W0' & Transfer ==40]
df4_control = df4_control[order(df4_control$exp_id),]
df4_control = df4_control[,c('seed','selected_function','Control_Maximum')]

df4 = df4[grep('round23',df4$exp_id)]
df4 = merge(rbindlist(lapply(df4$file,fread)),df4)
df4[,Maximum:=max(CommunityPhenotype),by=list(exp_id,Transfer)]
df4$Treatment = factor(df4$Treatment,levels=c('Bottleneck','Migration','Both'))
df4 = df4[Well == 'W0' & Transfer ==20]
df4 = df4[order(df4$exp_id),]
df4 = merge(df4,df4_control,by=c('seed','selected_function'))
df4$Q = df4$Maximum - df4$Control_Maximum
my_comparisons <-list( c("Bottleneck", "Migration"), c("Bottleneck", "Both"), c("Migration", "Both") )

#### Figure 4 ####
t_Synthetic_all = mapping_file3[grep('*iteration_5.*synthetic_community-migration',mapping_file3$exp_id)]
t_Synthetic_all$Community = 'Synthetic'
t_sel_all = mapping_file3[grep('*iteration_5.*selected_community-migration',mapping_file3$exp_id)]
t_sel_all$Community = "DE"
t_ctrl_all = mapping_file3[grep('*simple_screening.*selected_community-migration',mapping_file3$exp_id)]
t_ctrl_all$Community = "NS"
t_all2 = rbind(t_sel_all,t_Synthetic_all,t_ctrl_all)
t_all2 = t_all2[file.exists(t_all2$file)]
t_all2 = merge(rbindlist(lapply(t_all2$file,fread)),t_all2)
t_all2$Community = factor(t_all2$Community,levels=c("NS","DE",'Synthetic'))
df5 = t_all2[Transfer==40]
t20_2 = t_all2[Transfer==20]
# t20_2[t20_2$CommunityPhenotype <0,]$CommunityPhenotype  = 0
t20_2[,Fmax :=max(CommunityPhenotype),by=list(exp_id,Transfer)]

df5$RS = 1- (2*abs(t20_2$CommunityPhenotype-df5$CommunityPhenotype)/(t20_2$CommunityPhenotype+abs(t20_2$CommunityPhenotype-df5$CommunityPhenotype)))
df5[,Fmax :=max(CommunityPhenotype),by=list(exp_id,Transfer,selected_function)]
df5[,R :=mean(RS),by=list(exp_id,Transfer,selected_function)]
df5[,Fmean :=mean(CommunityPhenotype),by=list(exp_id,Transfer,selected_function)]
df5$Fmax2 = t20_2$Fmax
df5 = df5[Well=='W0']
my_comparisons2 <- list( c("NS", "DE"), c("DE", "Synthetic"), c("NS", "Synthetic") )


df1$selected_function = factor(df1$selected_function,levels = c('Additive',
                                                                # 'Additive Sparse',
                                                                'Non-Additive',
                                                                # 'Non-Additive Sparse',
                                                                'Costly',
                                                                'Target Resource',
                                                                'Invader Resistance'))
df2$selected_function = factor(df2$selected_function,levels = c('Additive',
                                                                # 'Additive Sparse',
                                                                'Non-Additive',
                                                                # 'Non-Additive Sparse',
                                                                'Costly',
                                                                'Target Resource',
                                                                'Invader Resistance'))
df3$selected_function = factor(df3$selected_function,levels = c('Additive',
                                                                # 'Additive Sparse',
                                                                'Non-Additive',
                                                                # 'Non-Additive Sparse',
                                                                'Costly',
                                                                'Target Resource',
                                                                'Invader Resistance'))
df4$selected_function = factor(df4$selected_function,levels = c('Additive',
                                                                # 'Additive Sparse',
                                                                'Non-Additive',
                                                                # 'Non-Additive Sparse',
                                                                'Costly',
                                                                'Target Resource',
                                                                'Invader Resistance'))
df5$selected_function = factor(df5$selected_function,levels = c('Additive',
                                                                # 'Additive Sparse',
                                                                'Non-Additive',
                                                                # 'Non-Additive Sparse',
                                                                'Costly',
                                                                'Target Resource',
                                                                'Invader Resistance'))


pA <- ggplot(df1,
             aes(x=protocol,y=Performance_Mean,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  geom_hline(yintercept=0,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90),axis.text.y =element_text(size=8),axis.title.y = element_text(size=10)) + 
  scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression( Mean(AS) - Mean(NS))) +
  facet_wrap(selected_function~.,scales='free_y',ncol=1,nrow=5) +
  scale_y_continuous(breaks=equal_breaks2(n=3, s=0.05))

pB <- ggplot(df1,
             aes(x=protocol,y=Performance_Max,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  geom_hline(yintercept=0,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x=element_text(size=10,angle=-90),axis.title.y = element_text(size=12)) + 
  scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression(Q==F[max](AS) - F[max](NS) )) + 
  facet_wrap(selected_function~.,scales='free_y',ncol=1,nrow=5) +scale_y_continuous(breaks=equal_breaks2(n=3, s=0.05))

pC <- ggplot() + 
  geom_line(df3,mapping = aes(x=Popsize_mean,y=Mean_F_Mean),col='Purple') +
  geom_ribbon(df3,mapping = aes(x=Popsize_mean,ymax=Mean_F_Mean +SD_F_Mean,ymin = Mean_F_Mean -SD_F_Mean),fill='Purple',alpha=0.2) +
  geom_line(df3,mapping = aes(x=Popsize_mean,y=Mean_F_Max),col='Orange') + 
  geom_ribbon(df3,mapping = aes(x=Popsize_mean,ymax=Mean_F_Max + SD_F_Max,ymin = Mean_F_Max - SD_F_Max),fill='Orange',alpha=0.2) + 
  xscale("log2",.format=FALSE) +
  labs(x = '<Population  Size> after bottleneck',
       y = 'F')  +
  theme_pubr()  + #labs(x='',y='') +
  # theme(axis.text = element_blank())   +
  facet_wrap(~selected_function,scales='free_y',ncol=1,nrow=5)  +scale_y_continuous(breaks=equal_breaks2(n=3, s=0.05))

pD <- ggplot(df2,
             aes(x=protocol,y=PO)) + 
  geom_boxplot(outlier.shape = NA,col='DarkGreen') +  
  geom_jitter(height=0,size=1,col='DarkGreen')  + 
  geom_hline(yintercept=0,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x=element_text(size=10,angle=-90),axis.title.y = element_text(size=12)) + 
  # scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  # scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression(F[max](Offspring) - F[max](Parent) )) +
  facet_wrap(selected_function~.,scales='free_y',ncol=1,nrow=5)  +scale_y_continuous(breaks=equal_breaks2(n=3, s=0.05))

pE <- ggboxplot(df4,x='Treatment',y='Q',col='Treatment',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white') + 
  stat_compare_means(paired=TRUE,comparisons = my_comparisons,method='t.test',size=3,
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) + 
  guides(col=FALSE) +   
  labs(x = '',y = 'Q') + 
  scale_colour_manual(values = c('#D95F02','#7570B3','#1B9E77')) + guides(fill=FALSE)+
  scale_x_discrete(labels =c('Bottleneck','Migration','Bottleneck +\n Migration'))+
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=-45,hjust=0.3),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))  +  
  facet_wrap(~selected_function,scales='free_y',ncol=1,nrow=5)  +
  scale_y_continuous(expand =c(0,0),breaks=equal_breaks2(n=3, s=0.1),limits=equal_limits(n=2,s=1))  

pF <- ggboxplot(df5,x='Community',y='Fmax2',col='Community',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white') + 
  stat_compare_means(paired=TRUE,comparisons = my_comparisons2,method='t.test',size=3,
                     # df5[df5$R<0]$R = 0
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) + 
  guides(col=FALSE) +   labs(x = '',y = expression(F[max])) +  
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3')) + guides(fill=FALSE)+
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=-45,hjust=0.3),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))  +  
  facet_wrap(~selected_function,scales='free_y',ncol=1,nrow=5)  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),breaks=equal_breaks(n=3, s=0.05)) 



pG <- ggboxplot(df5,x='Community',y='Fmean',col='Community',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white') + 
  stat_compare_means(paired=TRUE,comparisons = my_comparisons2,method='t.test',size=3,
                     # df5[df5$R<0]$R = 0
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) + 
  guides(col=FALSE) +   labs(x = '',y =  expression(Mean(F^'*')))  +  
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3')) + guides(fill=FALSE)+
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=-45,hjust=0.3),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))  +  
  facet_wrap(~selected_function,scales='free_y',ncol=1,nrow=5)  +  
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),breaks=equal_breaks(n=3, s=0.05)) 


pH <- ggboxplot(df5,x='Community',y='R',col='Community',palette = "dark2",
                add = "jitter",jitter.height=0,legend='right',shape=1,outlier.size=1,outlier.colour='white') + 
  stat_compare_means(paired=TRUE,comparisons = my_comparisons2,method='t.test',size=3,
                     # df5[df5$R<0]$R = 0
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("****", "***", "**", "*", "ns"))) + 
  guides(col=FALSE) +   labs(x = '',y = expression(Mean(R)))  +  
  scale_colour_manual(values = c('#1B9E77','#D95F02','#7570B3')) + guides(fill=FALSE)+
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=-45,hjust=0.3),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))  +  
  facet_wrap(~selected_function,scales='free_y',ncol=1,nrow=5)  + 
  scale_y_continuous(expand=c(0,0),breaks=equal_breaks2(n=3, s=0.05)) 

img1 =  readPNG( "../Plots/Cartoons/FigSX_functions.png", TRUE)
pcartoon <- ggarrange(rasterGrob(img1, interpolate=TRUE))

left = ggarrange(pcartoon,ggarrange(pB,pD,common.legend = TRUE,labels=c('B','C'),align='h'),ncol=2,nrow=1,labels=c('A',''),widths=c(1,1))
right = ggarrange(pE,pF,pG,ncol=3,nrow = 1,labels=c('D','E','F'), align = "h")
ggsave('../Plots/S12.png',ggarrange(left,right,nrow=2,ncol=1),height=16,width=12)