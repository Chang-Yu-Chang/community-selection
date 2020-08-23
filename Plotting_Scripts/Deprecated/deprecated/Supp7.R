rm(list=ls())
library(data.table)


# Read data
df_per_capita <- fread("../data/raw/per_capita_function/f1_additive.txt")
df_func <- fread("../data/temp/df_function_filtered_f1.txt")[Algorithm == "knock_out"]
df_comp <- fread("../data/temp/df_composition_perturbation.txt")[Algorithm == "knock_out" | Algorithm =='ctrl']

pre = df_comp[Algorithm == 'knock_out' & Transfer==20 & Type =='consumer',]
post = df_comp[Algorithm=='knock_out' & Transfer==40 & Type =='consumer',]
ctrl = df_comp[Algorithm=='ctrl' & Transfer ==40 & Type =='consumer',]

keep=c()
for(i in unique(df_func$SpeciesPool)){
  t = df_func[SpeciesPool==i & Transfer ==20]
  keep = c(keep,t[which(t$CommunityPhenotype == max(t$CommunityPhenotype))]$Well)
}

SpeciesPool = unique(df_func$SpeciesPool)
keep_df = data.frame(SpeciesPool,Well=keep)


ctrl_species = unique(Ctrl[SpeciesPool ==1]$ID)
pre[SpeciesPool==1 & Well == keep_df[SpeciesPool == 1]$Well,]