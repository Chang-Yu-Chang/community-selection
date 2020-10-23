# test plot the figures

library(tidyverse)
library(data.table)
library(cowplot)


input_independent_f1 <- fread("~/Desktop/Lab/community-selection/Data/Mapping_Files/input_independent_f1_additive.csv")
input_independent_f1a <- fread("~/Desktop/Lab/community-selection/Data/Mapping_Files/input_independent_f1a_additive.csv")


#list.files(path = "~/Dropbox/community-selection/Data/Raw_Rebuttal/")

df_ind_f1_func <- 
    input_independent_f1 %>%
    filter(protocol == "directed_selection") %>% 
    pull(exp_id) %>% 
    #input_independent_f1$exp_id %>%
    #grep("direct_selection", ., value = T) %>% 
    paste0("~/Dropbox/community-selection/Data/Raw_Rebuttal/", ., "_function.txt") %>%
    lapply(fread) %>% 
    rbindlist() %>%
    as_tibble()

df_ind_f1_func_max <- df_ind_f1_func %>%
    group_by(exp_id, Transfer) %>%
    filter(CommunityPhenotype == max(CommunityPhenotype))


df_ind_f1_func_max %>%
    filter(Transfer == 40) %>%
    left_join(input_independent_f1) %>% 
    filter(protocol == "directed_selection", bottleneck == T) %>%
    mutate(bottleneck_size = factor(bottleneck_size)) %>% 
    ggplot() +
    geom_boxplot(aes(x = bottleneck_size, y = CommunityPhenotype)) +
    theme_bw()

df_ind_f1_func_max %>% 
    mutate(seed = factor(seed)) %>%
    #filter(seed == 1:2) %>% 
    # filter(protocol == "simple_screening") %>% 
    ggplot() +
    geom_line(aes(x = Transfer, y = CommunityPhenotype, color = exp_id)) +
    facet_grid(seed ~. ) +
    theme_cowplot() +
    guides(color = F)
    

