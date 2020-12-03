suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(cowplot)))


df <- fread("../Data/Mapping_Files/input_iteration_f1_additive.csv")
df <- read_csv("../Data/Mapping_Files/input_robustness_f1_additive.csv")

df %>%
  filter(seed == 1) %>%
#  filter(grepl("iteration", exp_id)) %>%
#  filter(grepl("round1", exp_id)) %>%
  select(exp_id, n_migration)

view(df)

df1 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-simple_screening-1_function.txt")
df2 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-Blouin2015-1_function.txt")

df1 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-simple_screening-2_function.txt")
df2 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-Blouin2015-2_function.txt")

df1 %>%
  group_by(Transfer) %>%
  summarize(Fmax = max(CommunityPhenotype)) %>%
  filter(Transfer == 40)

df2 %>%
  group_by(Transfer) %>%
  summarize(Fmax = max(CommunityPhenotype)) %>%
  filter(Transfer == 40)



df1 %>%
  ggplot() +
  geom_line(aes(x = Transfer, y = CommunityPhenotype, color = Well)) +
  facet_grid(.~exp_id) +
  guides(color = F) +
  theme_cowplot()




df <- list.files("../Data/test/iteration_f5_invader_suppression/", pattern = "function", full.names = T) %>% 
    grep("-1_function", x = ., value = T) %>%
    grep("iteration_1", x = ., value = T) %>%
    lapply(fread) %>%
    rbindlist

df %>% 
    #filter(exp_id == "f5_invader_suppression-iteration_2_round2-3") %>% 
    ggplot() +
    geom_line(aes(x = Transfer, y = CommunityPhenotype, color = Well)) +
    facet_grid(.~exp_id) +
    guides(color = F) +
    theme_cowplot()
  

fread("../Data/test/independent_f1d_additive_medium2/f1d_additive_medium2-Blouin2015-1_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = Richness)) +
    theme_bw()







fread("../Data/test/independent_f1d_additive_medium2/f1d_additive_medium2-Blouin2015-1_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = Richness)) +
    theme_bw()

fread("../Data/test/independent_f1d_additive_medium2/f1d_additive_medium2-simple_screening-1_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = Richness)) +
    theme_bw()


df <- fread("~/Desktop/Lab/community-selection/Data/Mapping_Files/input_iteration_f5_invader_suppression.csv")
view(df)


fread("~/Dropbox/community-selection/Data/Raw_Rebuttal/f5_invader_suppression-iteration_1_round1-2_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = CommunityPhenotype)) +
    scale_y_continuous(limits = c(-500, 100)) +
    theme_bw()


fread("~/Dropbox/community-selection/Data/Raw_Rebuttal/f5_invader_suppression-iteration_1_round2-2_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = CommunityPhenotype)) +
    scale_y_continuous(limits = c(-500, 100)) +
    theme_bw()





fread("~/Dropbox/community-selection/Data/Raw_Rebuttal/f1b_additive_cost-iteration_1_round1-3_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = CommunityPhenotype)) +
    scale_y_continuous(limits = c(-1000, 1000)) +
    theme_bw()


fread("~/Dropbox/community-selection/Data/Raw_Rebuttal/f1b_additive_cost-iteration_1_round2-3_function.txt") %>%
    ggplot() +
    geom_line(aes(x = Transfer, group = Well, y = CommunityPhenotype)) +
    scale_y_continuous(limits = c(-1000, 1000)) +
    theme_bw()
