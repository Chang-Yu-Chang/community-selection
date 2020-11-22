suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))

fread("../Data/test/independent_f1d_additive_medium2/f1d_additive_medium2-Blouin2015-1_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = Richness)) +
    theme_bw()

fread("../Data/test/independent_f1d_additive_medium2/f1d_additive_medium2-simple_screening-1_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = Richness)) +
    theme_bw()
