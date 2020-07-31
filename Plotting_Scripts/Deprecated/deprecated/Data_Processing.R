rm(list=ls())
library(data.table)
library(ggplot2)
rm(list=ls())
folder1 = '../data/raw/perturbation/'
files1 = list.files(folder1,full.names=TRUE)
lst1 <- lapply(files1, fread)
function_df1 <- rbindlist(lst1)
fwrite(function_df1,file='../data/temp/aggregated_pertubation.csv')

rm(list=ls())
folder2 = '../data/raw/directed_selection/'
files2 = list.files(folder2,full.names=TRUE)
lst2 <- lapply(files2, fread)
function_df2 <- rbindlist(lst2)
fwrite(function_df2,file='../data/temp/aggregated_directed_selection.csv')

rm(list=ls())
folder3 = '../data/raw/literature/'
files3 = list.files(folder3,full.names=TRUE)
lst3 <- lapply(files3, fread)
function_df3 <- rbindlist(lst3)
fwrite(function_df3,file='../data/temp/aggregated_literature.csv')

rm(list=ls())
pertubations = fread('../data/temp/aggregated_pertubation.csv')
pertubations = fread('../data/temp/aggregated_pertubation.csv')