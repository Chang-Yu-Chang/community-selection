library(tidyverse)
library(data.table)
library(cowplot)
library(ggpubr)

df <- list.files(path = "~/Dropbox/community-selection/Data/Raw_Rebuttal/",
           pattern = "iteration_5_round1-|iteration_5_round20-", full.names = T) %>%
    grep("_composition.txt", ., value = T) %>%
    lapply(fread) %>%
    rbindlist() %>%
    filter(Type == "consumer", Transfer == 20) %>%
    separate(exp_id, into = c("SelectedFunction", "Treatment", "Seed"), sep = "-") %>%
    separate(Treatment, into = c("temp1", "temp2", "Round"), "_") %>%
    select(Round, Seed, Well, ID, Abundance)

df_mono <- list.files(path = "~/Dropbox/community-selection/Data/Raw_Rebuttal/", pattern = "monoculture-", full.names = T) %>%
    grep("f1_additive", ., value = T) %>%
    lapply(fread) %>%
    rbindlist() %>%
    mutate(Phi = CommunityPhenotype / Biomass, ID = sub("W", "", Well) %>% as.numeric()) %>%
    filter(Transfer == 20) %>%
    separate(exp_id, into = c("SelectedFunction", "Treatment", "Seed"), sep = "-") %>%
    select(Seed, ID, Phi, MonoAbundance = Biomass)



df_phi <- df %>%
    left_join(df_mono, by = c("Seed", "ID")) %>%
    mutate(PhiNi = Phi * Abundance)

df_selected <- df_phi %>%
    group_by(Round, Seed, Well) %>%
    summarize(CommunityPhenotype = sum(PhiNi)) %>%
    group_by(Round, Seed) %>%
    arrange(desc(CommunityPhenotype)) %>%
    slice(1)

df_plot <- df_selected %>%
    select(-CommunityPhenotype) %>%
    left_join(df_phi) %>%
    mutate(Seed = factor(Seed, 1:20)) %>%
    group_by(Round, Seed, Well) %>%
    mutate(RelativeAbundance = Abundance / sum(Abundance))

p1 <- df_plot %>%
    #filter(RelativeAbundance > 0.001) %>%
    ggboxplot(x = "Seed", y = "Phi", color = "Round", add = "jitter", add.params = list(size = 1)) +
    scale_color_manual(values = c('#1B9E77','#D95F02'), labels = c("round1"="Ancestral", "round20"="Directly evolved")) +
    stat_compare_means(aes(group = Round), label = "p.signif", method = "t.test") +
    theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_blank()) +
    labs(y = expression(phi[i]), x = "")

p2 <- df_plot %>%
    #filter(RelativeAbundance > 0.001) %>%
    ggboxplot(x = "Seed", y = "MonoAbundance", color = "Round", add = "jitter", add.params = list(size = 1)) +
    scale_color_manual(values = c('#1B9E77','#D95F02'), labels = c("round1"="Ancestral", "round20"="Directly evolved")) +
    stat_compare_means(aes(group = Round), label = "p.signif",  method = "t.test") +
    theme(legend.position = "none", legend.title = element_blank()) +
    labs(x = "Replicate", y = expression(N[i]~~(monoculture)))

p <- plot_grid(p1, p2, align = "v", axis = "lr", nrow = 2, labels = c("A", "B"))
ggsave("../Plots/FigS17.png", plot = p, width = 8, height = 7)




