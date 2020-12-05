suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(cowplot)))

list.files("../Data/test/independent_f5_invader_suppression/")

tested <- "independent_f5_invader_suppression"
mapping <- fread(paste0("../Data/Mapping_Files/input_", tested, ".csv"))
# which(grepl("screen", mapping$exp_id))-1
# which(grepl("Blouin2015-", mapping$exp_id))-1
df_func <- list.files(paste0("../Data/test/", tested,"/"), full.names = T) %>%
  lapply(fread) %>%
  rbindlist() %>%
  left_join(mapping)

df_func %>%
  filter(Transfer == 40) %>%
  ggplot(aes(x = exp_id, y = CommunityPhenotype)) +
  geom_boxplot() +
  geom_jitter(shape = 21) +
  facet_grid(seed~protocol, scales = "free_x") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90))


df_func %>%
  filter(Transfer == 40) %>%
  group_by(exp_id) %>%
  summarise(Fmax = max(CommunityPhenotype)) %>%
  separate(col = exp_id, into = c("selectedfunction", "protocol", "seed"), sep = "-") %>%
#  group_by(protocol, seed) %>%
  pivot_wider(names_from = protocol, values_from = Fmax) %>%
  mutate(Q = Blouin2015 - simple_screening) %>%
  select(seed, Q)
  ggplot(aes(x=Q)) + geom_histogram()


df_func %>%
  ggplot(aes(x = Transfer, y = CommunityPhenotype, group = Well)) +
  geom_line() +
  facet_grid(seed~protocol, scales = "free_x") +
  theme_cowplot()





mapping_independent <- fread("../Data/Mapping_Files/input_independent.csv")
mapping_iteration <- fread("../Data/Mapping_Files/input_iteration.csv")
mapping_robustness <- fread("../Data/Mapping_Files/input_robustness.csv")

mapping_iteration %>%
  #filter(grepl("iteration_\\d_round11", exp_id)) %>%
  filter(grepl("iteration_1", exp_id)) %>%
  select(exp_id, n_inoc, n_migration)

which(mapping_$exp_id == "f1_additive-iteration_1_round1-2")

which(mapping_iteration$exp_id == "f1_additive-iteration_1_round1-2")
which(mapping_iteration$exp_id == "f1_additive-iteration_1_round1-2")





fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-simple_screening-1_function.txt")
df2 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-Blouin2015-1_function.txt")

df1 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-simple_screening-2_function.txt")
df2 <- fread("../Data/test/independent_f5_invader_suppression/f5_invader_suppression-Blouin2015-2_function.txt")



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
