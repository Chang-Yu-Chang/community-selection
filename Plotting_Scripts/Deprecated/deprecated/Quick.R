rm(list=ls())
library(data.table)
library(ggplot2)

df = fread('../data/raw/botttleneck/bottleneck-1-f1_additive-TPreBottleneck-function.txt')
df2 = fread('../data/raw/botttleneck/bottleneck-1-f1_additive-TpostBottleneck-function.txt')
