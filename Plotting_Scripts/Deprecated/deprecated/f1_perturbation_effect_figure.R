library(tidyverse)
library(data.table)
library(cowplot)


# Read data
perturbation <- fread("../data/temp/perturbation.txt")
df_perturbation_BC <- fread("../data/temp/df_perturbation_BC.txt", stringsAsFactors = T) %>% as_tibble() %>%
  left_join(perturbation) %>%
  mutate(Perturbation = factor(Perturbation))
df_perturbation_function_time <- fread("../data/temp/df_perturbation_function_time.txt", stringsAsFactors = T) %>% as_tibble()


# Function over time
well = 1
p1 <- df_perturbation_function_time %>%
  filter(TargetWell == paste0("TW", well)) %>%
  filter(ResourceStrength == 0) %>%
  ggplot(aes(x = Transfer, y = CommunityPhenotype, color = Well, group = Well)) +
  geom_line() +
  facet_wrap(MigrationStrength~., labeller = label_both, nrow = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "", y = "additive function")

p2 <- df_perturbation_function_time %>%
  filter(TargetWell == paste0("TW", well)) %>%
  filter(MigrationStrength == 0) %>%
  ggplot(aes(x = Transfer, y = CommunityPhenotype, color = Well, group = Well)) +
  geom_line() +
  facet_wrap(ResourceStrength~., labeller = label_both, nrow = 1) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "transfer", y = "additive function")

p <- plot_grid(p1, p2, ncol = 1)

ggsave(paste0("../figure/f1_perturbation_effect_time_TW", well, ".pdf"), plot = p, width = 15, height = 6)
ggsave(paste0("../figure/f1_perturbation_effect_time_TW", well, ".png"), plot = p, width = 15, height = 6)

# Function density plot
well = 1

temp_range <- df_perturbation_function_time %>%
  filter(Transfer == 40, TargetWell == paste0("TW", well)) %>%
  filter(ResourceStrength == 0) %>% pull(CommunityPhenotype) %>% range()

p1 <- df_perturbation_function_time %>%
  filter(Transfer == 40, TargetWell == paste0("TW", well)) %>%
  filter(ResourceStrength == 0) %>%
  mutate(MigrationStrength = factor(MigrationStrength)) %>%
  ggplot(aes(x = CommunityPhenotype, group = Perturbation)) +
  geom_density(alpha = 0.5, aes(fill = MigrationStrength), color = NA) +
  scale_x_continuous(limits = temp_range, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), trans = "sqrt") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "additive function") +
  #  ggtitle("Migration perturbation") +
  NULL
p1

p2 <- df_perturbation_function_time %>%
  filter(Transfer == 40, TargetWell == paste0("TW", well)) %>%
  filter(MigrationStrength == 0) %>%
  mutate(ResourceStrength = factor(ResourceStrength)) %>%
  ggplot(aes(x = CommunityPhenotype, group = Perturbation)) +
  geom_density(alpha = 0.5, aes(fill = ResourceStrength), color = NA) +
  scale_x_continuous(limits = temp_range, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), trans = "sqrt") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "additive function") +
  #  ggtitle("Resource perturbation") +
  NULL
p2

p <- plot_grid(p1, p2, ncol = 2)
p
ggsave(paste0("../figure/f1_perturbation_effect_function.pdf"), plot = p, width = 10, height = 5)
ggsave(paste0("../figure/f1_perturbation_effect_function.png"), plot = p, width = 10, height = 5)




# BC density plot
p1 <- df_perturbation_BC %>%
  filter(ResourceStrength == 0) %>%
  mutate(MigrationStrength = factor(MigrationStrength)) %>%
  ggplot(aes(x = Perturbation, y = Dissimilarity)) +
  geom_boxplot() +
  geom_jitter(size = .5, height = 0) +
#  geom_density(aes(x = Dissimilarity, group = Perturbation), alpha = 0.5, aes(fill = MigrationStrength), color = NA) +
  #  facet_wrap(ResourceStrength~., labeller = label_both, nrow = 1, scale = "free_y") +
#  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
#  scale_y_continuous(expand = c(0,0), trans = "sqrt") +
  #scale_y_continuous(limits = c(0,1)) +
  scale_y_log10(limits = ) +
  theme_bw() +
  theme(legend.position = "top") +
#  labs(x = "Bray-Curtis Dissimilarity") +
#  ggtitle("Migration perturbation") +
  NULL
p1

p2 <- df_perturbation_BC %>%
  filter(MigrationStrength == 0) %>%
  mutate(ResourceStrength = factor(ResourceStrength)) %>%
  ggplot(aes(x = ResourceStrength, y = Dissimilarity)) +
  geom_boxplot() +
  geom_jitter(size = .5, height = 0) +
  #  geom_histogram() +
#  geom_density(alpha = 0.5, aes(fill = ResourceStrength), color = NA) +
#  facet_wrap(ResourceStrength~., labeller = label_both, nrow = 1, scale = "free_y") +
#  scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
#  scale_y_continuous(expand = c(0,0), trans = "sqrt") +
#  scale_y_continuous(limits = c(0,1)) +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position = "top") +
#  labs(x = "Bray-Curtis Dissimilarity") +
#  ggtitle("Resource perturbation") +
  NULL
p2

p <- plot_grid(p1, p2, ncol = 2)
p
ggsave(paste0("../figure/f1_perturbation_effect_BC.pdf"), plot = p, width = 10, height = 5)
ggsave(paste0("../figure/f1_perturbation_effect_BC.png"), plot = p, width = 10, height = 5)






