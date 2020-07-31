library(data.table)
library(ggplot2)
library(operators)
rm(list=ls())
folder = '../data/raw/example_run/'
files = list.files(folder,full.names=TRUE)
files_a = files[grepl('f1_species_pool',files)]
files_b = files[!grepl('f1_species_pool',files)]
lst_a<- lapply(files_a, fread)
lst_b<- lapply(files_b, fread)
function_dfa <- rbindlist(lst_a)
function_dfb <- rbindlist(lst_b)
fwrite(rbind(function_dfa,function_dfb[CommunityPhenotypeName != 'f1_additive' | Assembly %!in% function_dfa$Assembly]),
       file='../data/temp/aggregated_example.csv')

rm(list=ls())

folder = '../data/raw/literature/'
files = list.files(folder,full.names=TRUE)
files_a = files[grepl('f1_species_pool',files)]
files_b = files[!grepl('f1',files)]
lst_a<- lapply(files_a, fread)
lst_b<- lapply(files_b, fread)
function_dfa <- rbindlist(lst_a)
function_dfb <- rbindlist(lst_b)
fwrite(rbind(function_dfa,
             function_dfb[CommunityPhenotypeName != 'f1_additive' | Assembly %!in% function_dfa$Assembly]),
       file='../data/temp/aggregated_literature.csv')

rm(list=ls())
folder = '../data/raw/perturbation'
files = list.files(folder,full.names=TRUE,recursive = TRUE)
files_a = files[grepl('f1_species_pool',files)]
files_a = files_a[!grepl('deprecated',files_a)]

files_b = files[!grepl('f1_species_pool',files)]
files_b = files_b[!grepl('composition',files_b)]
lst_a<- lapply(files_a, fread)
lst_b<- lapply(files_b, fread)
function_dfa <- rbindlist(lst_a)
function_dfb <- rbindlist(lst_b)
fwrite(rbind(function_dfa,function_dfb[CommunityPhenotypeName != 'f1_additive' | Assembly %!in% function_dfa$Assembly]),
       file='../data/temp/aggregated_pertubation.csv')

rm(list=ls())
folder = '../data/raw/directed_selection/'
files = list.files(folder,full.names=TRUE)
files_a = files[grepl('f1_species_pool',files)]
files_b = files[!grepl('f1_species_pool',files)]
lst_a<- lapply(files_a, fread)
lst_b<- lapply(files_b, fread)
function_dfa <- rbindlist(lst_a)
function_dfb <- rbindlist(lst_b)
fwrite(rbind(function_dfa,function_dfb[CommunityPhenotypeName != 'f1_additive' | Assembly %!in% function_dfa$Assembly]),
       file='../data/temp/aggregated_directed_selection.csv')

#Split in 2 because files are too large
rm(list=ls())
folder4 = '../data/raw/long_experiment/'
files4 = list.files(folder4,full.names=TRUE)
lst4 <- lapply(files4[1:50], fread)
function_df4 <- rbindlist(lst4)
fwrite(function_df4,file='../data/temp/aggregated_long_experiment_1.csv')

rm(list=ls())
folder4 = '../data/raw/long_experiment/'
files4 = list.files(folder4,full.names=TRUE)
lst4 <- lapply(files4[50:length(files4)], fread)
function_df4 <- rbindlist(lst4)
fwrite(function_df4,file='../data/temp/aggregated_long_experiment_2.csv')

rm(list=ls())
a = fread('../data/temp/aggregated_directed_selection.csv')[CommunityPhenotypeName %in% c('f1_additive','f2_interaction','f2a_interaction')]
a$Type = 'DS'
b = fread('../data/temp/aggregated_literature.csv')[CommunityPhenotypeName %in% c('f1_additive','f2_interaction','f2a_interaction')]
b$Type = 'L'
c = fread('../data/temp/aggregated_pertubation.csv')[CommunityPhenotypeName %in% c('f1_additive','f2_interaction','f2a_interaction')]
c$Type = 'P'
d = fread('../data/temp/aggregated_example.csv')[CommunityPhenotypeName %in% c('f1_additive','f2_interaction','f2a_interaction')]
d$Type = 'E'
fwrite( rbind(a,b,c,d),file='../data/temp/aggregated_short.csv')
file.remove('../data/temp/aggregated_directed_selection.csv')
file.remove('../data/temp/aggregated_literature.csv')
file.remove('../data/temp/aggregated_pertubation.csv')
file.remove('../data/temp/aggregated_example.csv')

  
library(data.table)
library(ggplot2)
rm(list=ls())
a = fread('../data/temp/aggregated_long_experiment_1.csv')
b = fread('../data/temp/aggregated_long_experiment_2.csv')
function_df6 = rbindlist(list(a, b))
fwrite(function_df6, '../data/temp/aggregated_long.csv')
