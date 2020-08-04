rm(list=ls())
library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(png)
library(grid)
mapping_file = fread('../Data/Mapping_Files/input_independent.csv')
mapping_file$file = paste('../Data/Raw/',mapping_file$exp_id,'_function.txt',sep='')

mapping_file = mapping_file[cost_mean==0  & selected_function=='f1_additive',]
#For a single run and protocol
df_a = mapping_file[monoculture==FALSE & seed==5 & protocol %in% c('simple_screening',"Swenson2000a","Swenson2000a_control")]
df_a = merge(rbindlist(lapply(df_a$file,fread)),df_a)
df_a[,Maximum:=max(CommunityPhenotype),by=list(exp_id,Transfer)]
df_a[,Mean:=mean(CommunityPhenotype),by=list(exp_id,Transfer)]
df_a = df_a[Well == 'W0']

#For all runs and protocols
df_b = mapping_file[protocol %in% c('Blouin2015','Jochum2019',
                                    'Mueller2019','Panke_Buisse2015',
                                    'Swenson2000a','Swenson2000b',
                                    'Chang2020a',
                                    'Chang2020b','Arora2019',
                                    'Raynaud2019a','Raynaud2019b','Wright2019')]
df_b$protocol = factor(df_b$protocol,levels=c('Swenson2000a','Blouin2015',
                                              'Panke_Buisse2015','Jochum2019',
                                              'Raynaud2019b','Mueller2019',
                                              'Wright2019','Swenson2000b','Arora2019','Raynaud2019a','Chang2020a','Chang2020b'))
df_b$Performance_Max = NA
df_b$Performance_Mean = NA
for(j in 1:nrow(df_b)){
  k = df_b$seed[j]
  screen = fread(mapping_file[seed==k & protocol=='simple_screening' & monoculture==FALSE]$file)
  ref_max = max(screen[Transfer==40]$CommunityPhenotype)
  ref_mean = mean(screen[Transfer==40]$CommunityPhenotype)
  temp_df = fread(df_b$file[j])[Transfer==40]
  df_b$Performance_Max[j] = max(temp_df$CommunityPhenotype) -ref_max
  df_b$Performance_Mean[j] = mean(temp_df$CommunityPhenotype) - ref_mean
}

df_b$Method = 'Migrant Pool'
df_b[protocol%in% c('Chang2020a','Chang2020b','Swenson2000b','Arora2019','Raynaud2019a')]$Method = 'Propagule'


img1 =  readPNG( "../Plots/Cartoons/Fig1A.png", TRUE)
img2 = readPNG( "../Plots/Cartoons/Fig1B.png", TRUE)
pA <- rasterGrob(img1, interpolate=TRUE)
pB <- rasterGrob(img2, interpolate=TRUE)

pC <- ggplot(df_a[Transfer>0,])  +
  annotate("rect",xmin=-Inf,xmax=20.5,ymin=-Inf,ymax=Inf,alpha=0.2,fill='Grey') +
  annotate("text",x=10.5,y=550,label='Selection',size=4) +
  annotate("text",x=30.5,y=550,label='Stabilization',size=4) + 
  geom_line(aes(x=Transfer,y=Mean,col=protocol),size=0.5)+ 
  geom_point(aes(x=Transfer,y=Mean,col=protocol),size=1) +
  labs(y = expression(Mean(F)),x = 'Generation',col='') + 
  theme_pubr() + scale_y_continuous(limits=c(0,570), breaks = c(0,500)) +
  scale_x_continuous(breaks = c(0,20,40)) + guides(col=FALSE)+
  scale_colour_brewer(palette='Dark2',labels=c('NS','AS','RS'))+
  theme(axis.text = element_text(size=9),axis.title = element_text(size=12)) + 
  theme(axis.title.y = element_text(margin= margin(t = 0, r = 0, b = 0, l = 0)))

pD <- ggplot(df_a[Transfer>0,])  +
  annotate("rect",xmin=-Inf,xmax=20.5,ymin=-Inf,ymax=Inf,alpha=0.2,fill='Grey') +
  annotate("text",x=10.5,y=550,label='Selection',size=4) +
  annotate("text",x=30.5,y=550,label='Stabilization',size=4) + 
  geom_line(aes(x=Transfer,y=Maximum,col=protocol),size=0.5)+ 
  geom_point(aes(x=Transfer,y=Maximum,col=protocol),size=1) +
  labs(y = expression(F[max]),x = 'Generation',col='') + guides(col=FALSE)+
  theme_pubr() + scale_y_continuous(limits=c(0,570), breaks = c(0,500)) +
  scale_x_continuous(breaks = c(0,20,40)) +
  scale_colour_brewer(palette='Dark2',labels=c('NS','AS','RS')) +
  theme(axis.text = element_text(size=9),axis.title = element_text(size=12))+ 
  theme(axis.title.y = element_text(margin= margin(t = 0, r = 5, b = 0, l = 0)))


pE <- ggplot(df_b,
             aes(x=protocol,y=Performance_Mean,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90),axis.text.y =element_text(size=8),axis.title.y = element_text(size=10)) + 
  scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression( Mean(AS) - Mean(NS))) + scale_y_continuous(breaks=c(2000,0,-2000),limits=c(-2000,2000))

pF <- ggplot(df_b,
               aes(x=protocol,y=Performance_Max,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0,size=1)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x=element_text(size=10,angle=-90),axis.text.y =element_text(size=8),axis.title.y = element_text(size=10)) + 
  scale_shape_manual(values=c(2,1)) + labs(col='',shape='') +
  scale_colour_manual(values = c('Orange','Purple')) + 
  # geom_text(y=1500,label = "***",size=3) +
  labs(x = '',y=expression(Q==F[max](AS) - F[max](NS) )) + scale_y_continuous(breaks=c(0,-2000,2000),limits=c(-2000,2000))

top = ggarrange(pA,labels=c('A'),ncol=1)
middle = ggarrange( ggarrange(pB,labels=c('B'),ncol=1),ggarrange(pC,pD,labels=c('C','D'),vjust =1,nrow=2,ncol=1),widths=c(2.2,1))
bottom = ggarrange(pE,pF,common.legend=TRUE,labels=c('E','F'),legend='top',vjust=1)

ggsave('../Plots/Fig1.png',ggarrange(top,middle,bottom,ncol=1,heights = c(1.5,2,2)),height=11,width=11)


#ATest that all significant
protocol=c()
p_mean = c()
p_max= c()
for(k in unique(df_b$protocol)){
  protocol=c(protocol,k)
  p_mean = c(p_mean,t.test(df_b[protocol==k]$Performance_Mean)$p.value)
  p_max = c(p_max,t.test(df_b[protocol==k]$Performance_Max)$p.value)
}
