library(data.table)
library(tidyverse)
library(cowplot)

# Algorithm
algorithms <- fread("../data/temp/algorithms.txt", stringsAsFactors = T)

# Read performance data
df_performance <- fread("../data/temp/df_performance.txt", stringsAsFactors = T) %>%
  filter(AlgorithmCategory != "perturbation") %>%
  filter(!(CommunityPhenotypeName %in% c("f3_additive_binary", "f4_interaction_binary"))) %>%
#  filter(grepl("f1|f2|f2a|f5|f6", CommunityPhenotypeName)) %>%
  mutate(Algorithm = factor(Algorithm, levels = algorithms$Algorithm)) %>%
  as.data.table()


"
Because both f5 and f6 are negative, so the performance that computes the ratio does no work
"
df_performance[grepl("f5|f6", CommunityPhenotypeName),
              ":="(Max_Performance = -Max_Performance+2,
                    Max_Performance_monoculture = -Max_Performance_monoculture+2)]

# Plot the performance
p1 <- df_performance %>%
  filter(grepl("f1|f2|f5|f6", CommunityPhenotypeName)) %>%
  filter(!(Algorithm %in% c("Blouin2015_control", "Swenson2000a_control", "monoculture"))) %>%
  filter(Transfer == 35) %>%
  ggplot(aes(x = Algorithm, y = Max_Performance, color = AlgorithmCategory)) +
  geom_boxplot(lwd = .5, outlier.size = 0.5) +
  geom_hline(yintercept = 1) +
  geom_jitter(size = .5) +
  facet_grid(CommunityPhenotypeName~., scale = "free_y") +
  theme_half_open() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.title = element_blank(), legend.position = "top") +
  panel_border() +
  labs(x = "", y = "performance")

p1a <- df_performance %>%
  filter(grepl("f1|f2|f5|f6", CommunityPhenotypeName)) %>%
  filter(!(Algorithm %in% c("Blouin2015_control", "Swenson2000a_control", "monoculture"))) %>%
  filter(Transfer == 40) %>%
  ggplot(aes(x = Algorithm, y = Max_Performance_monoculture, color = AlgorithmCategory)) +
  geom_boxplot(lwd = .5, outlier.size = 0.5) +
  geom_hline(yintercept = 1) +
  geom_jitter(size = .5) +
  facet_grid(CommunityPhenotypeName~., scale = "free_y") +
  theme_half_open() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.title = element_blank(), legend.position = "top") +
  panel_border() +
  labs(x = "", y = "performance")

p <- plot_grid(p1, p1a, labels = c("A", "B"))
ggsave("../figure/algorithm_performance.pdf", width = 15, height = 15); p; invisible(dev.off())
ggsave("../figure/algorithm_performance.png", width = 15, height = 15); p; invisible(dev.off())


# Histogram
p2 <- df_performance %>%
  filter(Transfer == 40) %>%
  filter(grepl("f1|f2|f2a|f5|f6|f7", CommunityPhenotypeName)) %>%
  filter(Algorithm != "simple_screening") %>%
  ggplot(aes(x = Max_Performance, fill = AlgorithmCategory)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = 2, color = "red") +
  facet_grid(Algorithm~CommunityPhenotypeName, scales = "free") +
  theme_half_open() +
  panel_border() +
  #  theme(legend.position = "top") +
  labs(x = "performance", y = "density")

p2
ggsave("../figure/algorithm_performance_histogram.pdf", width = 15, height = 20); p2; invisible(dev.off())
ggsave("../figure/algorithm_performance_histogram.png", width = 15, height = 20); p2; invisible(dev.off())


# Temporal dynamics
# p3 <- df_performance %>%
#   filter(Algorithm %in% c("Swenson2000b", "Mueller2019", "pair_top_communities")) %>%
#   filter(SpeciesPool %in% 1) %>%
# #  pivot_longer(cols = ends_with("Performance"), names_to = "SummaryStat", values_to = "Performance") %>%
#   ggplot(aes(x = Transfer, y = Max_Performance, fill = Algorithm)) +
#   geom_rect(xmin = 0, xmax = 20, ymin = -Inf, ymax = Inf, color = NA, fill = grey(level = .9, alpha = .9)) +
#   geom_point(aes(color = Algorithm)) +
#   geom_line(aes(color = Algorithm)) +
# #  geom_segment(aes(xend = Transfer, y = MeanTheta1 - SdTheta1, yend = MeanTheta1 + SdTheta1, color = Algorithm)) +
#   #  geom_ribbon(aes(ymin = MeanTheta1 - SdTheta1, ymax = MeanTheta1 + SdTheta1), alpha = 0.2) +
#   facet_grid(CommunityPhenotypeName~SpeciesPool, scales = "free_y") +
#   scale_x_continuous(expand = c(0, 0)) +
#   theme_half_open() +
#   theme(legend.position = "top") +
#   panel_border() +
#   labs(x = "time", y = "performance")
#
# p3
# ggsave("figure/algorithm_performance_time.pdf", width = 8, height = 20); p3; invisible(dev.off())
# ggsave("figure/algorithm_performance_time.png", width = 8, height = 20); p3; invisible(dev.off())


# Temporal dynamics
p4 <- df_performance %>%
  filter(Algorithm %in% c("Swenson2000b", "Mueller2019", "pair_top_communities")) %>%
  group_by(CommunityPhenotypeName, Transfer, Algorithm) %>%
  summarize(SdMaxPerformance = sd(Max_Performance), MeanMaxPerformance = mean(Max_Performance),
            MaxMaxPerformance = max(Max_Performance), MinMaxPerformance = min(Max_Performance),
            MedianMaxPerformance = median(Max_Performance)) %>%
  ggplot(aes(x = Transfer, y = MedianMaxPerformance, fill = Algorithm)) +
  geom_rect(xmin = 0, xmax = 20, ymin = -Inf, ymax = Inf, color = NA, fill = grey(level = .9, alpha = .9)) +
  geom_point(aes(color = Algorithm)) +
  geom_line(aes(color = Algorithm)) +
  #  geom_segment(aes(xend = Transfer, y = MeanTheta1 - SdTheta1, yend = MeanTheta1 + SdTheta1, color = Algorithm)) +
#  geom_ribbon(aes(ymin = MeanMaxPerformance - SdMaxPerformance, ymax = MeanMaxPerformance + SdMaxPerformance), alpha = 0.2) +
  geom_ribbon(aes(ymin = MinMaxPerformance, ymax = MaxMaxPerformance), alpha = 0.2) +
  facet_grid(CommunityPhenotypeName~., scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_half_open() +
  theme(legend.position = "top") +
  panel_border() +
  labs(x = "time", y = "performance")

p4
ggsave("../figure/algorithm_performance_time.pdf", width = 8, height = 20); p4; invisible(dev.off())
ggsave("../figure/algorithm_performance_time.png", width = 8, height = 20); p4; invisible(dev.off())














