# Plot rarefaction curve for three metacommunity sampling methods
library(tidyverse)
library(cowplot)
library(data.table)

calculate_rarefaction_curve <- function(plate_N, sample_sizes = c(5, 1e1, 50, 1e2, 500, seq(1e3, 1e4, 1e3), seq(2e4, 1e5, 1e4)), n_replicate = 100) {
    community_abundance <- plate_N %>%
        select(W0, W1, W2, W3, W4, W5, W6, W7, W8, W9) %>%
        mutate(ID = 1:nrow(.)) %>%
        pivot_longer(starts_with("W"), names_to = "Community", values_to = "Abundance") %>%
        filter(Abundance != 0) %>%
        mutate(Abundance = Abundance * 1e6) %>%
        arrange(Community)

    communities <- unique(community_abundance$Community)
    temp_list <- rep(list(tibble(Community = rep(communities, each = length(sample_sizes)), SampleSize = rep(sample_sizes, length(communities)))), Richness = NA, n_replicate)

    for (i in 1:length(communities)) {
        for (j in 1:length(sample_sizes)) {
            for (b in 1:n_replicate) {
                temp = community_abundance %>% filter(Community == communities[i])
                one_sample <- sample(rep(temp$ID, times = temp$Abundance), size = sample_sizes[[j]], replace = F)
                richness = length(unique(one_sample))
                temp_list[[b]][temp_list[[b]]$Community == communities[i] & temp_list[[b]]$SampleSize == sample_sizes[[j]], "Richness"] <- richness
            }
        }
        cat("\n", i)
    }
    
    temp_list %>% 
        rbindlist(idcol = "Replicate") %>%
        return()
}

plate_power <- fread("../Data/test/plate_power.txt")
plate_lognormal <- fread("../Data/test/plate_lognormal.txt")
plate_default <- fread("../Data/test/plate_default.txt")

rarefaction_power <- calculate_rarefaction_curve(plate_power) 
rarefaction_lognormal <- calculate_rarefaction_curve(plate_lognormal) 
rarefaction_default <- calculate_rarefaction_curve(plate_default) 
    

p1 <- rarefaction_power %>% 
    group_by(Community, SampleSize) %>%
    summarize(MeanRichness = mean(Richness)) %>%
    ggplot(aes(x = SampleSize, y = MeanRichness, color = Community)) +
    geom_line() +
    theme_cowplot() +
    guides(color = F) +
    ggtitle("Power")

p2 <- rarefaction_lognormal %>% 
    group_by(Community, SampleSize) %>%
    summarize(MeanRichness = mean(Richness)) %>%
    ggplot(aes(x = SampleSize, y = MeanRichness, color = Community)) +
    geom_line() +
    theme_cowplot() +
    guides(color = F) +
    ggtitle("Lognormal")
    
p3 <- rarefaction_default %>% 
    group_by(Community, SampleSize) %>%
    summarize(MeanRichness = mean(Richness)) %>%
    ggplot(aes(x = SampleSize, y = MeanRichness, color = Community)) +
    geom_line() +
    theme_cowplot() +
    ggtitle("Default")


p <- plot_grid(p1, p2, p3, nrow = 1, align = "hv", axis = "tblr")
ggsave("../Plots/rarefaction.png", width = 12, height = 4)




