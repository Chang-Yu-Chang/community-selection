library(data.table)
library(ggpubr)
library(ggplot2)
library(gridExtra)

mapping_file = fread('../data/input_additive.csv')
mapping_file$file = paste('../data/raw/',mapping_file$exp_id,'_function.txt',sep='')


df_b = mapping_file[protocol %in% c('Blouin2015','Jochum2019',
                             'Mueller2019','Panke_Buisse2015',
                             'Swenson2000a','Swenson2000b',
                             'Swenson2000c','Chang2020a',
                             'Chang2020b','Arora2019',
                             'Raynaud2019a','Raynaud2019b','Wright2019','simple_screening') & monoculture==FALSE]
df_b[protocol=='simple_screening']$protocol= 'Screen'
df_b$protocol = factor(df_b$protocol,levels=c('Swenson2000a','Swenson2000c','Blouin2015',
                                              'Panke_Buisse2015','Jochum2019',
                                              'Raynaud2019b','Mueller2019',
                                              'Wright2019','Swenson2000b','Arora2019','Raynaud2019a','Chang2020a','Chang2020b','Screen'))

df_b$Performance_Max = NA
df_b$Performance_Mean = NA

for(j in 1:nrow(df_b)){
    k = df_b$seed[j]
    screen = fread(mapping_file[seed==k & protocol=='simple_screening' & monoculture==TRUE]$file)
    ref_max = max(screen[Transfer==40]$CommunityPhenotype)
    ref_mean = mean(screen[Transfer==40]$CommunityPhenotype)
    temp_df = fread(df_b$file[j])[Transfer==40]
    df_b$Performance_Max[j] = max(temp_df$CommunityPhenotype) -ref_max
    df_b$Performance_Mean[j] = mean(temp_df$CommunityPhenotype) - ref_mean
    
}

df_b$Method = 'Pooling'
df_b[protocol%in% c('Chang2020a','Chang2020b','Swenson2000b','Arora2019','Raynaud2019a')]$Method = 'Propagule'
df_b[protocol == 'Screen']$Method = 'Screen'
p1 <- ggplot(df_b,
             aes(x=protocol,y=Performance_Max,col=Method,shape=Method)) + 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(height=0)  + 
  geom_hline(yintercept=1,linetype=2,col='Red')  + theme_pubr()  +
  theme(axis.text.x =element_text(size=10,angle=-90)) + 
  scale_shape_manual(values=c(3,1,2)) + 
  scale_colour_manual(values = c('Orange','Purple','Grey')) + geom_text(y=1.2,label = "***",size=3) +
  labs(x = '',y=expression(F[max*(Protocol)] - F[max*(monoculture)]))


ggsave('../Plots/Supp1.png',p1,height=5,width=5)

